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
#include "db/name-inserter.h"
#include "db/query.h"
#include "db/reach-driver.h"
#include "db/statement-driver.hh"
#include "modelpath.h"
#include "sqlite3.h"
#include "uuidgen.h"

using std::cerr;
using std::endl;
using std::sprintf;
using std::strcmp;
using std::strcpy;
using std::string;
using std::strlen;

namespace flint {
namespace {

const char kTreeQuery[] = "SELECT DISTINCT component FROM variables";

class TreeDumper {
public:
	explicit TreeDumper(sqlite3 *db)
		: query_stmt_(NULL),
		  insert_stmt_(NULL),
		  cm_()
	{
		int e;
		if (!CreateTable(db, "spaces", "(space_id BLOB, name TEXT)"))
			exit(EXIT_FAILURE);
		if (!CreateTable(db, "names", "(space_id BLOB, type TEXT, id INTEGER, name TEXT, unit TEXT, capacity REAL)"))
			exit(EXIT_FAILURE);
		if (!CreateTable(db, "reaches", "(output_uuid BLOB, output_id INTEGER, input_uuid BLOB, input_id INTEGER, reduction INTEGER)"))
			exit(EXIT_FAILURE);
		if (!CreateView(db, "scopes", "SELECT space_id AS uuid, space_id, NULL AS label FROM spaces"))
			exit(EXIT_FAILURE);
		e = sqlite3_prepare_v2(db, kTreeQuery, -1, &query_stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << kTreeQuery << endl;
			exit(EXIT_FAILURE);
		}
		e = sqlite3_prepare_v2(db, "INSERT INTO spaces VALUES (?, ?)",
							   -1, &insert_stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
	}

	~TreeDumper() {
		sqlite3_finalize(query_stmt_);
		sqlite3_finalize(insert_stmt_);
	}

	bool Dump(const boost::filesystem::path &path) {
		std::unique_ptr<UuidGenerator> gen(new UuidGenerator(path));
		int e;
		for (e = sqlite3_step(query_stmt_); e == SQLITE_ROW; e = sqlite3_step(query_stmt_)) {
			const unsigned char *c = sqlite3_column_text(query_stmt_, 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			boost::uuids::uuid uuid = (*gen)();
			e = sqlite3_bind_blob(insert_stmt_, 1, &uuid, uuid.size(), SQLITE_STATIC);
			if (e != SQLITE_OK) {
				cerr << "failed to bind uuid: " << e << endl;
				return false;
			}
			e = sqlite3_bind_text(insert_stmt_, 2, (const char *)c, -1, SQLITE_STATIC);
			if (e != SQLITE_OK) {
				cerr << "failed to bind name: " << e << endl;
				return false;
			}
			e = sqlite3_step(insert_stmt_);
			if (e != SQLITE_DONE) {
				cerr << "failed to step statement: " << e << endl;
				return false;
			}
			sqlite3_reset(insert_stmt_);
			cm_.insert(std::make_pair(string((const char *)c), uuid));
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

	bool Find(const char *c, boost::uuids::uuid *u) const {
		ComponentMap::const_iterator it = cm_.find(string(c));
		if (it == cm_.end()) {
			cerr << "could not find component named "
				 << c
				 << endl;
			return false;
		}
		*u = it->second;
		return true;
	}

private:
	typedef std::unordered_map<string, boost::uuids::uuid> ComponentMap;

	sqlite3_stmt *query_stmt_;
	sqlite3_stmt *insert_stmt_;
	ComponentMap cm_;
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

const char kOdeQuery[] = "SELECT component, body FROM maths WHERE body LIKE ' (eq (diff (bvar ~%time) ~%%' ESCAPE '~'";

class OdeDumper : public db::StatementDriver {
public:
	OdeDumper(sqlite3 *db, const TreeDumper *tree_dumper)
		: db::StatementDriver(db, kOdeQuery)
		, ei_(db)
		, tree_dumper_(tree_dumper)
		, dvm_()
	{
	}

	bool Dump() {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *c = sqlite3_column_text(stmt(), 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *body = sqlite3_column_text(stmt(), 1);
			size_t nlen = strlen((const char *)body);
			if (nlen == 0) {
				cerr << "empty body" << endl;
				return false;
			}
			if (!tree_dumper_->Find((const char *)c, &u)) return false;

			if (!ei_.Insert(u, (const char *)&body[1])) return false;

			// register it as a dependent variable
			std::unique_ptr<char[]> tail(new char[nlen+1]);
			strcpy(tail.get(), (const char *)&body[kPrefixLength]);
			for (size_t i=0;i<nlen-kPrefixLength;i++) {
				if (tail[i] == ')') {
					tail[i] = '\0';
					break;
				}
			}
			dvm_[(const char *)c].insert(tail.get());
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
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
	typedef std::unordered_map<string, std::unordered_set<string> > DependentVariableMap;

	EqInserter ei_;
	const TreeDumper *tree_dumper_;
	DependentVariableMap dvm_;
};

const char kNameQuery[] = "SELECT rowid, component, name, initial_value FROM variables";

class NameDumper : public db::NameInserter, public db::StatementDriver {
public:
	NameDumper(sqlite3 *db, const TreeDumper *tree_dumper)
		: db::NameInserter("names", db)
		, db::StatementDriver(db, kNameQuery)
		, tree_dumper_(tree_dumper)
	{
	}

	bool Dump(const OdeDumper *ode_dumper) {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			int id = static_cast<int>(sqlite3_column_int64(stmt(), 0));
			const unsigned char *c = sqlite3_column_text(stmt(), 1);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *n = sqlite3_column_text(stmt(), 2);
			size_t nlen = strlen((const char *)n);
			if (nlen == 0) {
				cerr << "empty variable name" << endl;
				return false;
			}
			const unsigned char *iv = sqlite3_column_text(stmt(), 3);
			if (!tree_dumper_->Find((const char *)c, &u)) return false;
			if (strcmp("time", (const char *)n) == 0) {
				// ignore "time", because we suppose all of them are equivalent
				// to the system "time"
			} else if (iv) {
				if (ode_dumper->IsDependentVariable((const char *)c, (const char *)n)) {
					if (!InsertName(u, 'x', id, (const char *)n)) return false;
				} else {
					if (!InsertName(u, 's', id, (const char *)n)) return false;
				}
			} else {
				if (!InsertName(u, 'v', id, (const char *)n)) return false;
			}
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	const TreeDumper *tree_dumper_;
};

const char kIvQuery[] = "SELECT component, name, initial_value FROM variables WHERE initial_value IS NOT NULL";

class IvDumper : public db::StatementDriver {
public:
	IvDumper(sqlite3 *db, const TreeDumper *tree_dumper)
		: db::StatementDriver(db, kIvQuery)
		, ii_(db)
		, tree_dumper_(tree_dumper)
	{
	}

	bool Dump() {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *c = sqlite3_column_text(stmt(), 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *n = sqlite3_column_text(stmt(), 1);
			size_t nlen = strlen((const char *)n);
			if (nlen == 0) {
				cerr << "empty variable name" << endl;
				return false;
			}
			const unsigned char *iv = sqlite3_column_text(stmt(), 2);
			if (!tree_dumper_->Find((const char *)c, &u)) return false;

			size_t blen = strlen((const char *)n) + strlen((const char *)iv);
			std::unique_ptr<char[]> buf(new char[blen+8]);
			sprintf(buf.get(), "(eq %%%s %s)", (const char *)n, (const char *)iv);
			if (!ii_.Insert(u, buf.get())) return false;
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	IvInserter ii_;
	const TreeDumper *tree_dumper_;
};

const char kFunctionQuery[] = "SELECT component, body FROM maths WHERE body LIKE ' (eq ~%%' ESCAPE '~'";

class FunctionDumper : public db::StatementDriver {
public:
	FunctionDumper(sqlite3 *db, const TreeDumper *tree_dumper)
		: db::StatementDriver(db, kFunctionQuery)
		, ei_(db)
		, ii_(db)
		, tree_dumper_(tree_dumper)
	{
	}

	bool Dump() {
		int e;
		boost::uuids::uuid u;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *c = sqlite3_column_text(stmt(), 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *body = sqlite3_column_text(stmt(), 1);
			size_t nlen = strlen((const char *)body);
			if (nlen == 0) {
				cerr << "empty body" << endl;
				return false;
			}
			if (!tree_dumper_->Find((const char *)c, &u)) return false;

			const char *math = (const char *)&body[1];
			if (!ei_.Insert(u, math)) return false;
			if (!ii_.Insert(u, math)) return false;
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	EqInserter ei_;
	IvInserter ii_;
	const TreeDumper *tree_dumper_;
};

const char kReachQuery[] = "SELECT "
 "v1.component, v1.rowid, v1.name, v1.public_interface, v1.private_interface, "
 "v2.component, v2.rowid, v2.name, v2.public_interface, v2.private_interface FROM connections AS c "
 "LEFT JOIN map_variables AS mv ON c.rowid = mv.connection_id "
 "LEFT JOIN variables AS v1 ON c.component_1 = v1.component AND mv.variable_1 = v1.name "
 "LEFT JOIN variables AS v2 ON c.component_2 = v2.component AND mv.variable_2 = v2.name "
 "WHERE v1.name != 'time' AND v2.name != 'time'"; // ignore any mapping from "time" to "time"

class ReachDumper : public db::StatementDriver {
public:
	ReachDumper(sqlite3 *db, const TreeDumper *tree_dumper)
		: db::StatementDriver(db, kReachQuery)
		, driver_(new db::ReachDriver(db))
		, tree_dumper_(tree_dumper)
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
			if (!tree_dumper_->Find((const char *)c1, &u1)) return false;
			if (!tree_dumper_->Find((const char *)c2, &u2)) return false;

			if ( ( (pub1 && strcmp((const char *)pub1, "out") == 0) ||
				   (pri1 && strcmp((const char *)pri1, "out") == 0) ) &&
				 ( (pub2 && strcmp((const char *)pub2, "in") == 0) ||
				   (pri2 && strcmp((const char *)pri2, "in") == 0) ) ) {
				if (!driver_->Save(u1, id1, u2, id2, Reduction::kSum))
					return false;
			} else if ( ( (pub1 && strcmp((const char *)pub1, "in") == 0) ||
						  (pri1 && strcmp((const char *)pri1, "in") == 0) ) &&
						( (pub2 && strcmp((const char *)pub2, "out") == 0) ||
						  (pri2 && strcmp((const char *)pri2, "out") == 0) ) ) {
				if (!driver_->Save(u2, id2, u1, id1, Reduction::kSum))
					return false;
			} else {
				cerr << "invalid variable mapping found: "
					 << c1 << '|' << n1 << '|' << pub1 << '|' << pri1
					 << ' '
					 << c2 << '|' << n2 << '|' << pub2 << '|' << pri2
					 << endl;
				return false;
			}
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	std::unique_ptr<db::ReachDriver> driver_;
	const TreeDumper *tree_dumper_;
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
	if (!CreateTable(db, "input_eqs", "(uuid BLOB, math TEXT)"))
		return false;

	std::unique_ptr<TreeDumper> tree_dumper(new TreeDumper(db));
	if (!tree_dumper->Dump(path)) return false;

	std::unique_ptr<OdeDumper> ode_dumper(new OdeDumper(db, tree_dumper.get()));
	if (!ode_dumper->Dump()) return false;

	std::unique_ptr<NameDumper> name_dumper(new NameDumper(db, tree_dumper.get()));
	if (!name_dumper->Dump(ode_dumper.get())) return false;

	std::unique_ptr<IvDumper> iv_dumper(new IvDumper(db, tree_dumper.get()));
	if (!iv_dumper->Dump()) return false;

	std::unique_ptr<FunctionDumper> function_dumper(new FunctionDumper(db, tree_dumper.get()));
	if (!function_dumper->Dump()) return false;

	std::unique_ptr<ReachDumper> reach_dumper(new ReachDumper(db, tree_dumper.get()));
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
