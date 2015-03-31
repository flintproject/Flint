/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <set>
#include <sstream>

#include <boost/ptr_container/ptr_map.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include "db/driver.h"
#include "db/reach-driver.h"
#include "modelpath.h"
#include "sqlite3.h"
#include "uuidgen.h"

using std::cerr;
using std::endl;
using std::strcmp;
using std::strcpy;
using std::string;
using std::strlen;

static const size_t kUuidLength = 36;

namespace {

const char kTreeQuery[] = "SELECT DISTINCT component FROM variables";

class TreeDumper {
public:
	explicit TreeDumper(sqlite3 *db)
		: query_stmt_(NULL),
		  insert_stmt_(NULL),
		  cm_()
	{
		char *em;
		int e;
		e = sqlite3_exec(db, "CREATE TABLE IF NOT EXISTS spaces (uuid TEXT, name TEXT)",
						 NULL, NULL, &em);
		if (e != SQLITE_OK) {
			cerr << "failed to create table spaces: " << e << endl;
			exit(EXIT_FAILURE);
		}
		e = sqlite3_exec(db, "CREATE TABLE IF NOT EXISTS names (space_id TEXT, type TEXT, id INTEGER, name TEXT, unit TEXT, capacity REAL)",
						 NULL, NULL, &em);
		if (e != SQLITE_OK) {
			cerr << "failed to create table names: " << e << endl;
			exit(EXIT_FAILURE);
		}
		e = sqlite3_exec(db, "CREATE TABLE IF NOT EXISTS reaches (output_uuid BLOB, output_id INTEGER, input_uuid BLOB, input_id INTEGER)",
						 NULL, NULL, &em);
		if (e != SQLITE_OK) {
			cerr << "failed to create table reaches: " << e << endl;
			exit(EXIT_FAILURE);
		}
		e = sqlite3_exec(db, "CREATE VIEW IF NOT EXISTS scopes AS SELECT uuid, uuid, NULL FROM spaces",
						 NULL, NULL, &em);
		if (e != SQLITE_OK) {
			cerr << "failed to create view scopes: " << e << endl;
			exit(EXIT_FAILURE);
		}
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
		boost::scoped_ptr<UuidGenerator> gen(new UuidGenerator(path));
		int e;
		for (e = sqlite3_step(query_stmt_); e == SQLITE_ROW; e = sqlite3_step(query_stmt_)) {
			const unsigned char *c = sqlite3_column_text(query_stmt_, 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			string uuid = (*gen)();
			e = sqlite3_bind_text(insert_stmt_, 1, (const char *)uuid.c_str(), -1, SQLITE_STATIC);
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

	bool Find(const char *c, char *u) const {
		ComponentMap::const_iterator it = cm_.find(string(c));
		if (it == cm_.end()) {
			cerr << "could not find component named "
				 << c
				 << endl;
			return false;
		}
		strcpy(u, it->second.c_str());
		return true;
	}

private:
	typedef std::map<string, string> ComponentMap;

	sqlite3_stmt *query_stmt_;
	sqlite3_stmt *insert_stmt_;
	ComponentMap cm_;
};

const char kOdeQuery[] = "SELECT component, body FROM maths WHERE body LIKE ' (eq (diff (bvar ~%time) ~%%' ESCAPE '~'";

class OdeDumper {
public:
	OdeDumper(const char *path, sqlite3 *db, const TreeDumper *tree_dumper)
		: fp_(fopen(path, "w")),
		  tree_dumper_(tree_dumper),
		  stmt_(),
		  dvm_()
	{
		int e = sqlite3_prepare_v2(db, kOdeQuery, -1, &stmt_, NULL);
		if (e != SQLITE_OK) stmt_ = NULL;
	}

	~OdeDumper() {
		if (stmt_) sqlite3_finalize(stmt_);
		if (fp_) fclose(fp_);
	}

	bool Dump() {
		if (!stmt_) {
			cerr << "failed to prepare statement: " << kOdeQuery << endl;
			return false;
		}
		int e;
		char u[kUuidLength+1];
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *c = sqlite3_column_text(stmt_, 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *body = sqlite3_column_text(stmt_, 1);
			size_t nlen = strlen((const char *)body);
			if (nlen == 0) {
				cerr << "empty body" << endl;
				return false;
			}
			if (!tree_dumper_->Find((const char *)c, u)) return false;
			fprintf(fp_, "%s%s\n", u, (const char *)body);

			// register it as a dependent variable
			boost::scoped_array<char> tail(new char[nlen+1]);
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
		return it->second->find(name) != it->second->end();
	}

private:
	static const size_t kPrefixLength = 25; // length of " (eq (diff (bvar %time) %"
	typedef boost::ptr_map<string, std::set<string> > DependentVariableMap;

	FILE *fp_;
	const TreeDumper *tree_dumper_;
	sqlite3_stmt *stmt_;
	DependentVariableMap dvm_;
};

const char kNameQuery[] = "SELECT rowid, component, name, initial_value FROM variables";

class NameDumper {
public:
	NameDumper(const char *path, sqlite3 *db, const TreeDumper *tree_dumper)
		: fp_(fopen(path, "w")),
		  tree_dumper_(tree_dumper),
		  stmt_()
	{
		int e = sqlite3_prepare_v2(db, kNameQuery, -1, &stmt_, NULL);
		if (e != SQLITE_OK) stmt_ = NULL;
	}

	~NameDumper() {
		if (stmt_) sqlite3_finalize(stmt_);
		if (fp_) fclose(fp_);
	}

	bool Dump(const OdeDumper *ode_dumper) {
		if (!stmt_) {
			cerr << "failed to prepare statement: " << kNameQuery << endl;
			return false;
		}
		int e;
		char u[kUuidLength+1];
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			int id = static_cast<int>(sqlite3_column_int64(stmt_, 0));
			const unsigned char *c = sqlite3_column_text(stmt_, 1);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *n = sqlite3_column_text(stmt_, 2);
			size_t nlen = strlen((const char *)n);
			if (nlen == 0) {
				cerr << "empty variable name" << endl;
				return false;
			}
			const unsigned char *iv = sqlite3_column_text(stmt_, 3);
			if (!tree_dumper_->Find((const char *)c, u)) return false;
			if (strcmp("time", (const char *)n) == 0) {
				// ignore "time", because we suppose all of them are equivalent
				// to the system "time"
			} else if (iv) {
				if (ode_dumper->IsDependentVariable((const char *)c, (const char *)n)) {
					fprintf(fp_, "%s x %d %s\n", u, id, (const char *)n);
				} else {
					fprintf(fp_, "%s s %d %s\n", u, id, (const char *)n);
				}
			} else {
				fprintf(fp_, "%s v %d %s\n", u, id, (const char *)n);
			}
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	FILE *fp_;
	const TreeDumper *tree_dumper_;
	sqlite3_stmt *stmt_;
};

const char kIvQuery[] = "SELECT component, name, initial_value FROM variables WHERE initial_value IS NOT NULL";

class IvDumper {
public:
	IvDumper(const char *path, sqlite3 *db, const TreeDumper *tree_dumper)
		: fp_(fopen(path, "w")),
		  tree_dumper_(tree_dumper),
		  stmt_()
	{
		int e = sqlite3_prepare_v2(db, kIvQuery, -1, &stmt_, NULL);
		if (e != SQLITE_OK) stmt_ = NULL;
	}

	~IvDumper() {
		if (stmt_) sqlite3_finalize(stmt_);
		if (fp_) fclose(fp_);
	}

	bool Dump() {
		if (!stmt_) {
			cerr << "failed to prepare statement: " << kIvQuery << endl;
			return false;
		}
		int e;
		char u[kUuidLength+1];
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *c = sqlite3_column_text(stmt_, 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *n = sqlite3_column_text(stmt_, 1);
			size_t nlen = strlen((const char *)n);
			if (nlen == 0) {
				cerr << "empty variable name" << endl;
				return false;
			}
			const unsigned char *iv = sqlite3_column_text(stmt_, 2);
			if (!tree_dumper_->Find((const char *)c, u)) return false;
			fprintf(fp_, "%s (eq %%%s %s)\n", u, (const char *)n, (const char *)iv);
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	FILE *fp_;
	const TreeDumper *tree_dumper_;
	sqlite3_stmt *stmt_;
};

const char kFunctionQuery[] = "SELECT component, body FROM maths WHERE body LIKE ' (eq ~%%' ESCAPE '~'";

class FunctionDumper {
public:
	FunctionDumper(const char *path, sqlite3 *db, const TreeDumper *tree_dumper)
		: fp_(fopen(path, "w")),
		  tree_dumper_(tree_dumper),
		  stmt_()
	{
		int e = sqlite3_prepare_v2(db, kFunctionQuery, -1, &stmt_, NULL);
		if (e != SQLITE_OK) stmt_ = NULL;
	}

	~FunctionDumper() {
		if (stmt_) sqlite3_finalize(stmt_);
		if (fp_) fclose(fp_);
	}

	bool Dump() {
		if (!stmt_) {
			cerr << "failed to prepare statement: " << kFunctionQuery << endl;
			return false;
		}
		int e;
		char u[kUuidLength+1];
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *c = sqlite3_column_text(stmt_, 0);
			size_t clen = strlen((const char *)c);
			if (clen == 0) {
				cerr << "empty component name" << endl;
				return false;
			}
			const unsigned char *body = sqlite3_column_text(stmt_, 1);
			size_t nlen = strlen((const char *)body);
			if (nlen == 0) {
				cerr << "empty body" << endl;
				return false;
			}
			if (!tree_dumper_->Find((const char *)c, u)) return false;
			fprintf(fp_, "%s%s\n", u, (const char *)body);
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	FILE *fp_;
	const TreeDumper *tree_dumper_;
	sqlite3_stmt *stmt_;
};

const char kReachQuery[] = "SELECT "
 "v1.component, v1.rowid, v1.name, v1.public_interface, v1.private_interface, "
 "v2.component, v2.rowid, v2.name, v2.public_interface, v2.private_interface FROM connections AS c "
 "LEFT JOIN map_variables AS mv ON c.rowid = mv.connection_id "
 "LEFT JOIN variables AS v1 ON c.component_1 = v1.component AND mv.variable_1 = v1.name "
 "LEFT JOIN variables AS v2 ON c.component_2 = v2.component AND mv.variable_2 = v2.name "
 "WHERE v1.name != 'time' AND v2.name != 'time'"; // ignore any mapping from "time" to "time"

class ReachDumper {
public:
	ReachDumper(sqlite3 *db, const TreeDumper *tree_dumper)
		: driver_(new db::ReachDriver(db)),
		  gen_(),
		  tree_dumper_(tree_dumper),
		  stmt_()
	{
		int e = sqlite3_prepare_v2(db, kReachQuery, -1, &stmt_, NULL);
		if (e != SQLITE_OK) stmt_ = NULL;
	}

	~ReachDumper() {
		if (stmt_) sqlite3_finalize(stmt_);
	}

	bool Dump() {
		if (!stmt_) {
			cerr << "failed to prepare statement: " << kReachQuery << endl;
			return false;
		}
		int e;
		char u1[kUuidLength+1], u2[kUuidLength+1];
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *c1 = sqlite3_column_text(stmt_, 0);
			int id1 = static_cast<int>(sqlite3_column_int64(stmt_, 1));
			const unsigned char *n1 = sqlite3_column_text(stmt_, 2);
			const unsigned char *pub1 = sqlite3_column_text(stmt_, 3);
			const unsigned char *pri1 = sqlite3_column_text(stmt_, 4);
			const unsigned char *c2 = sqlite3_column_text(stmt_, 5);
			int id2 = static_cast<int>(sqlite3_column_int64(stmt_, 6));
			const unsigned char *n2 = sqlite3_column_text(stmt_, 7);
			const unsigned char *pub2 = sqlite3_column_text(stmt_, 8);
			const unsigned char *pri2 = sqlite3_column_text(stmt_, 9);
			if (!tree_dumper_->Find((const char *)c1, u1)) return false;
			if (!tree_dumper_->Find((const char *)c2, u2)) return false;

			if ( ( (pub1 && strcmp((const char *)pub1, "out") == 0) ||
				   (pri1 && strcmp((const char *)pri1, "out") == 0) ) &&
				 ( (pub2 && strcmp((const char *)pub2, "in") == 0) ||
				   (pri2 && strcmp((const char *)pri2, "in") == 0) ) ) {
				if (!driver_->Save(gen_(u1), id1, gen_(u2), id2)) return false;
			} else if ( ( (pub1 && strcmp((const char *)pub1, "in") == 0) ||
						  (pri1 && strcmp((const char *)pri1, "in") == 0) ) &&
						( (pub2 && strcmp((const char *)pub2, "out") == 0) ||
						  (pri2 && strcmp((const char *)pri2, "out") == 0) ) ) {
				if (!driver_->Save(gen_(u2), id2, gen_(u1), id1)) return false;
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
	boost::scoped_ptr<db::ReachDriver> driver_;
	boost::uuids::string_generator gen_;
	const TreeDumper *tree_dumper_;
	sqlite3_stmt *stmt_;
};

void Usage()
{
	cerr << "usage: flint-cellmltr DB NAME IV FUNCTION ODE" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != 6) {
		Usage();
		return EXIT_FAILURE;
	}

	boost::scoped_array<char> filename(GetModelFilename(argv[1]));
	boost::filesystem::path path(filename.get());

	boost::scoped_ptr<db::Driver> driver(new db::Driver(argv[1]));

	boost::scoped_ptr<TreeDumper> tree_dumper(new TreeDumper(driver->db()));
	if (!tree_dumper->Dump(path)) return EXIT_FAILURE;

	boost::scoped_ptr<OdeDumper> ode_dumper(new OdeDumper(argv[5], driver->db(), tree_dumper.get()));
	if (!ode_dumper->Dump()) return EXIT_FAILURE;

	boost::scoped_ptr<NameDumper> name_dumper(new NameDumper(argv[2], driver->db(), tree_dumper.get()));
	if (!name_dumper->Dump(ode_dumper.get())) return EXIT_FAILURE;

	boost::scoped_ptr<IvDumper> iv_dumper(new IvDumper(argv[3], driver->db(), tree_dumper.get()));
	if (!iv_dumper->Dump()) return EXIT_FAILURE;

	boost::scoped_ptr<FunctionDumper> function_dumper(new FunctionDumper(argv[4], driver->db(), tree_dumper.get()));
	if (!function_dumper->Dump()) return EXIT_FAILURE;

	boost::scoped_ptr<ReachDumper> reach_dumper(new ReachDumper(driver->db(), tree_dumper.get()));
	if (!reach_dumper->Dump()) return EXIT_FAILURE;

	return EXIT_SUCCESS;
}
