/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "translator.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

#include <boost/uuid/uuid.hpp>

#include "db/eq-inserter.h"
#include "db/helper.h"
#include "db/query.h"
#include "db/reach-driver.h"
#include "db/statement-driver.h"
#include "db/variable-inserter.h"
#include "modelpath.h"
#include "sqlite3.h"
#include "uuidgen.h"

using std::sprintf;
using std::strcmp;
using std::strcpy;
using std::strlen;

namespace flint {
namespace {

class ComponentMap {
public:
	void Insert(const unsigned char *name, const boost::uuids::uuid &uuid) {
		map_.emplace(std::string(reinterpret_cast<const char *>(name)), uuid);
	}

	bool Find(const char *c, boost::uuids::uuid *u) const {
		auto it = map_.find(std::string(c));
		if (it == map_.end()) {
			std::cerr << "could not find component named "
				 << c
				 << std::endl;
			return false;
		}
		*u = it->second;
		return true;
	}

private:
	std::unordered_map<std::string, boost::uuids::uuid> map_;
};

class TreeDumper {
public:
	TreeDumper()
		: query_stmt_(nullptr),
		  insert_stmt_(nullptr)
	{
	}

	~TreeDumper() {
		sqlite3_finalize(query_stmt_);
		sqlite3_finalize(insert_stmt_);
	}

	bool Dump(sqlite3 *db, const boost::filesystem::path &path, ComponentMap *cm) {
		static const char kTreeQuery[] = "SELECT DISTINCT component FROM cellml_variables";

		int e;
		if (!CreateTable(db, "spaces", "(space_id BLOB, name TEXT)"))
			return false;
		if (!CreateTable(db, "variables", VARIABLES_SCHEMA))
			return false;
		if (!CreateTable(db, "reaches", REACHES_SCHEMA))
			return false;
		if (!CreateView(db, "scopes", "SELECT space_id AS uuid, space_id, NULL AS label FROM spaces"))
			return false;
		e = sqlite3_prepare_v2(db, kTreeQuery, -1, &query_stmt_, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << kTreeQuery << std::endl;
			return false;
		}
		e = sqlite3_prepare_v2(db, "INSERT INTO spaces VALUES (?, ?)",
							   -1, &insert_stmt_, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			return false;
		}

		std::unique_ptr<UuidGenerator> gen(new UuidGenerator(path));
		for (e = sqlite3_step(query_stmt_); e == SQLITE_ROW; e = sqlite3_step(query_stmt_)) {
			const unsigned char *c = sqlite3_column_text(query_stmt_, 0);
			size_t clen = strlen(reinterpret_cast<const char *>(c));
			if (clen == 0) {
				std::cerr << "empty component name" << std::endl;
				return false;
			}
			boost::uuids::uuid uuid = (*gen)();
			e = sqlite3_bind_blob(insert_stmt_, 1, &uuid, uuid.size(), SQLITE_STATIC);
			if (e != SQLITE_OK) {
				std::cerr << "failed to bind uuid: " << e << std::endl;
				return false;
			}
			e = sqlite3_bind_text(insert_stmt_, 2, reinterpret_cast<const char *>(c), -1, SQLITE_STATIC);
			if (e != SQLITE_OK) {
				std::cerr << "failed to bind name: " << e << std::endl;
				return false;
			}
			e = sqlite3_step(insert_stmt_);
			if (e != SQLITE_DONE) {
				std::cerr << "failed to step statement: " << e << std::endl;
				return false;
			}
			sqlite3_reset(insert_stmt_);
			cm->Insert(c, uuid);
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		return true;
	}

private:
	sqlite3_stmt *query_stmt_;
	sqlite3_stmt *insert_stmt_;
};

class IvInserter : public db::EqInserter {
public:
	explicit IvInserter(sqlite3 *db)
		: db::EqInserter("input_ivs", db)
	{}
};

class EqInserter : public db::EqInserter {
public:
	explicit EqInserter(sqlite3 *db)
		: db::EqInserter("input_eqs", db)
	{}
};

const char kOdeQuery[] = "SELECT component, body FROM cellml_maths WHERE body LIKE ' (eq (diff (bvar ~%time) ~%%' ESCAPE '~'";

class OdeDumper : public db::StatementDriver {
public:
	OdeDumper(sqlite3 *db, const ComponentMap *cm)
		: db::StatementDriver(db, kOdeQuery)
		, ei_(db)
		, cm_(cm)
		, dvm_()
	{
	}

	bool Dump() {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *c = sqlite3_column_text(stmt(), 0);
			size_t clen = strlen(reinterpret_cast<const char *>(c));
			if (clen == 0) {
				std::cerr << "empty component name" << std::endl;
				return false;
			}
			const unsigned char *body = sqlite3_column_text(stmt(), 1);
			size_t nlen = strlen(reinterpret_cast<const char *>(body));
			if (nlen == 0) {
				std::cerr << "empty body" << std::endl;
				return false;
			}
			if (!cm_->Find(reinterpret_cast<const char *>(c), &u)) return false;

			if (!ei_.Insert(u, reinterpret_cast<const char *>(&body[1]))) return false;

			// register it as a dependent variable
			std::unique_ptr<char[]> tail(new char[nlen+1]);
			strcpy(tail.get(), reinterpret_cast<const char *>(&body[kPrefixLength]));
			for (size_t i=0;i<nlen-kPrefixLength;i++) {
				if (tail[i] == ')') {
					tail[i] = '\0';
					break;
				}
			}
			dvm_[reinterpret_cast<const char *>(c)].insert(tail.get());
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		return true;
	}

	bool IsDependentVariable(const char *component, const char *name) const {
		DependentVariableMap::const_iterator it = dvm_.find(component);
		if (it == dvm_.end()) return false;
		return it->second.find(name) != it->second.end();
	}

private:
	static const size_t kPrefixLength = 25; // length of " (eq (diff (bvar %time) %"
	typedef std::unordered_map<std::string, std::unordered_set<std::string> > DependentVariableMap;

	EqInserter ei_;
	const ComponentMap *cm_;
	DependentVariableMap dvm_;
};

class VariableDumper : public db::VariableInserter, public db::StatementDriver {
public:
	VariableDumper(sqlite3 *db, const ComponentMap *cm)
		: db::VariableInserter("variables", true, db) // TODO: real independency
		, db::StatementDriver(db, "SELECT rowid, component, name, initial_value FROM cellml_variables")
		, cm_(cm)
	{
	}

	bool Dump(const OdeDumper *ode_dumper) {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			int id = static_cast<int>(sqlite3_column_int64(stmt(), 0));
			const unsigned char *c = sqlite3_column_text(stmt(), 1);
			size_t clen = strlen(reinterpret_cast<const char *>(c));
			if (clen == 0) {
				std::cerr << "empty component name" << std::endl;
				return false;
			}
			const unsigned char *n = sqlite3_column_text(stmt(), 2);
			size_t nlen = strlen(reinterpret_cast<const char *>(n));
			if (nlen == 0) {
				std::cerr << "empty variable name" << std::endl;
				return false;
			}
			const unsigned char *iv = sqlite3_column_text(stmt(), 3);
			if (!cm_->Find(reinterpret_cast<const char *>(c), &u)) return false;
			if (strcmp("time", reinterpret_cast<const char *>(n)) == 0) {
				// ignore "time", because we suppose all of them are equivalent
				// to the system "time"
			} else if (iv) {
				if (ode_dumper->IsDependentVariable(reinterpret_cast<const char *>(c), reinterpret_cast<const char *>(n))) {
					if (!Insert(u, 'x', id, reinterpret_cast<const char *>(n))) return false;
				} else {
					if (!Insert(u, 's', id, reinterpret_cast<const char *>(n))) return false;
				}
			} else {
				if (!Insert(u, 'v', id, reinterpret_cast<const char *>(n))) return false;
			}
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		return true;
	}

private:
	const ComponentMap *cm_;
};

const char kIvQuery[] = "SELECT component, name, initial_value FROM cellml_variables WHERE initial_value IS NOT NULL";

class IvDumper : public db::StatementDriver {
public:
	IvDumper(sqlite3 *db, const ComponentMap *cm)
		: db::StatementDriver(db, kIvQuery)
		, ii_(db)
		, cm_(cm)
	{
	}

	bool Dump() {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *c = sqlite3_column_text(stmt(), 0);
			size_t clen = strlen(reinterpret_cast<const char *>(c));
			if (clen == 0) {
				std::cerr << "empty component name" << std::endl;
				return false;
			}
			const unsigned char *n = sqlite3_column_text(stmt(), 1);
			size_t nlen = strlen(reinterpret_cast<const char *>(n));
			if (nlen == 0) {
				std::cerr << "empty variable name" << std::endl;
				return false;
			}
			const unsigned char *iv = sqlite3_column_text(stmt(), 2);
			if (!cm_->Find(reinterpret_cast<const char *>(c), &u)) return false;

			size_t blen = strlen(reinterpret_cast<const char *>(n)) + strlen(reinterpret_cast<const char *>(iv));
			std::unique_ptr<char[]> buf(new char[blen+8]);
			sprintf(buf.get(), "(eq %%%s %s)", reinterpret_cast<const char *>(n), reinterpret_cast<const char *>(iv));
			if (!ii_.Insert(u, buf.get())) return false;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		return true;
	}

private:
	IvInserter ii_;
	const ComponentMap *cm_;
};

const char kFunctionQuery[] = "SELECT component, body FROM cellml_maths WHERE body LIKE ' (eq ~%%' ESCAPE '~'";

class FunctionDumper : public db::StatementDriver {
public:
	FunctionDumper(sqlite3 *db, const ComponentMap *cm)
		: db::StatementDriver(db, kFunctionQuery)
		, ei_(db)
		, ii_(db)
		, cm_(cm)
	{
	}

	bool Dump() {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *c = sqlite3_column_text(stmt(), 0);
			size_t clen = strlen(reinterpret_cast<const char *>(c));
			if (clen == 0) {
				std::cerr << "empty component name" << std::endl;
				return false;
			}
			const unsigned char *body = sqlite3_column_text(stmt(), 1);
			size_t nlen = strlen(reinterpret_cast<const char *>(body));
			if (nlen == 0) {
				std::cerr << "empty body" << std::endl;
				return false;
			}
			if (!cm_->Find(reinterpret_cast<const char *>(c), &u)) return false;

			const char *math = reinterpret_cast<const char *>(&body[1]);
			if (!ei_.Insert(u, math)) return false;
			if (!ii_.Insert(u, math)) return false;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		return true;
	}

private:
	EqInserter ei_;
	IvInserter ii_;
	const ComponentMap *cm_;
};

const char kReachQuery[] = "SELECT "
 "v1.component, v1.rowid, v1.name, v1.public_interface, v1.private_interface, "
 "v2.component, v2.rowid, v2.name, v2.public_interface, v2.private_interface FROM cellml_connections AS c "
 "LEFT JOIN cellml_map_variables AS mv ON c.rowid = mv.connection_id "
 "LEFT JOIN cellml_variables AS v1 ON c.component_1 = v1.component AND mv.variable_1 = v1.name "
 "LEFT JOIN cellml_variables AS v2 ON c.component_2 = v2.component AND mv.variable_2 = v2.name "
 "WHERE v1.name != 'time' AND v2.name != 'time'"; // ignore any mapping from "time" to "time"

class ReachDumper : public db::StatementDriver {
public:
	ReachDumper(sqlite3 *db, const ComponentMap *cm)
		: db::StatementDriver(db, kReachQuery)
		, driver_(new db::ReachDriver(db))
		, cm_(cm)
	{
	}

	bool Dump() {
		int e;
		boost::uuids::uuid u1, u2;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *c1 = sqlite3_column_text(stmt(), 0);
			int id1 = static_cast<int>(sqlite3_column_int64(stmt(), 1));
			const unsigned char *n1 = sqlite3_column_text(stmt(), 2);
			const unsigned char *pub1 = sqlite3_column_text(stmt(), 3);
			const unsigned char *pri1 = sqlite3_column_text(stmt(), 4);
			const unsigned char *c2 = sqlite3_column_text(stmt(), 5);
			int id2 = static_cast<int>(sqlite3_column_int64(stmt(), 6));
			const unsigned char *n2 = sqlite3_column_text(stmt(), 7);
			const unsigned char *pub2 = sqlite3_column_text(stmt(), 8);
			const unsigned char *pri2 = sqlite3_column_text(stmt(), 9);
			if (!cm_->Find(reinterpret_cast<const char *>(c1), &u1)) return false;
			if (!cm_->Find(reinterpret_cast<const char *>(c2), &u2)) return false;

			if ( ( (pub1 && strcmp(reinterpret_cast<const char *>(pub1), "out") == 0) ||
				   (pri1 && strcmp(reinterpret_cast<const char *>(pri1), "out") == 0) ) &&
				 ( (pub2 && strcmp(reinterpret_cast<const char *>(pub2), "in") == 0) ||
				   (pri2 && strcmp(reinterpret_cast<const char *>(pri2), "in") == 0) ) ) {
				if (!driver_->Save(u1, id1, u2, id2, Reduction::kSum))
					return false;
			} else if ( ( (pub1 && strcmp(reinterpret_cast<const char *>(pub1), "in") == 0) ||
						  (pri1 && strcmp(reinterpret_cast<const char *>(pri1), "in") == 0) ) &&
						( (pub2 && strcmp(reinterpret_cast<const char *>(pub2), "out") == 0) ||
						  (pri2 && strcmp(reinterpret_cast<const char *>(pri2), "out") == 0) ) ) {
				if (!driver_->Save(u2, id2, u1, id1, Reduction::kSum))
					return false;
			} else {
				std::cerr << "invalid variable mapping found: "
					 << c1 << '|' << n1 << '|' << pub1 << '|' << pri1
					 << ' '
					 << c2 << '|' << n2 << '|' << pub2 << '|' << pri2
					 << std::endl;
				return false;
			}
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		return true;
	}

private:
	std::unique_ptr<db::ReachDriver> driver_;
	const ComponentMap *cm_;
};

} // namespace

bool TranslateCellml(sqlite3 *db)
{
	std::unique_ptr<char[]> filename(GetModelFilename(db));
	boost::filesystem::path path(filename.get());

	if (!BeginTransaction(db))
		return false;

	if (!CreateTable(db, "input_ivs", "(uuid BLOB, math TEXT)"))
		return false;
	if (!CreateTable(db, "dependent_ivs", "(uuid BLOB, math TEXT)")) // TODO: insert proper rows
		return false;
	if (!CreateTable(db, "input_eqs", "(uuid BLOB, math TEXT)"))
		return false;

	std::unique_ptr<ComponentMap> cm(new ComponentMap);
	{
		TreeDumper dumper;
		if (!dumper.Dump(db, path, cm.get()))
			return false;
	}

	std::unique_ptr<OdeDumper> ode_dumper(new OdeDumper(db, cm.get()));
	if (!ode_dumper->Dump()) return false;

	{
		std::unique_ptr<VariableDumper> dumper(new VariableDumper(db, cm.get()));
		if (!dumper->Dump(ode_dumper.get())) return false;
	}

	std::unique_ptr<IvDumper> iv_dumper(new IvDumper(db, cm.get()));
	if (!iv_dumper->Dump()) return false;

	std::unique_ptr<FunctionDumper> function_dumper(new FunctionDumper(db, cm.get()));
	if (!function_dumper->Dump()) return false;

	std::unique_ptr<ReachDumper> reach_dumper(new ReachDumper(db, cm.get()));
	if (!reach_dumper->Dump()) return false;

	if (!CreateLayout(db))
		return false;
	if (!CreateSprinkles(db))
		return false;
	if (!CreateTsfiles(db))
		return false;
	if (!CreateConfig(db))
		return false;

	return CommitTransaction(db);
}

}
