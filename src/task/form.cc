/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "task.h"

#include <cassert>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <boost/uuid/uuid.hpp>

#include "db/query.h"
#include "db/eq-inserter.h"
#include "db/variable-inserter.h"
#include "sqlite3.h"

using std::cerr;
using std::endl;

namespace flint {
namespace task {

namespace {

class Inserter : db::VariableInserter {
public:
	explicit Inserter(sqlite3 *db)
		: db::VariableInserter("variables", db)
		, id_(1)
	{}

	bool Insert(const char *name) {
		return db::VariableInserter::Insert('s', id_++, name);
	}

private:
	int id_;
};

int InsertParameterName(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);
	Inserter *inserter = static_cast<Inserter *>(data);
	return inserter->Insert(argv[0]) ? 0 : 1;
}

int InsertTargetName(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);
	Inserter *inserter = static_cast<Inserter *>(data);
	int rowid = atoi(argv[0]);
	char name[64];
	sprintf(name, "phsp:target%d", rowid);
	return inserter->Insert(name) ? 0 : 1;
}

bool InsertNames(sqlite3 *db)
{
	Inserter inserter(db);
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT name FROM phsp_parameters", InsertParameterName, &inserter, &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			cerr << "failed to select phsp_parameters: " << e << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	e = sqlite3_exec(db, "SELECT rowid FROM phsp_targets", InsertTargetName, &inserter, &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			cerr << "failed to select phsp_targets: " << e << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

int InsertTargetEquation(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 2);
	db::EqInserter *inserter = static_cast<db::EqInserter *>(data);
	int rowid = atoi(argv[0]);
	const char *math = argv[1];
	std::unique_ptr<char[]> eqn(new char[strlen(math) + 32]);
	sprintf(eqn.get(), "(eq %%phsp:target%d%s)", rowid, math);
	if (!inserter->Insert(eqn.get())) {
		return 1;
	}
	return 0;
}

bool InsertEquations(sqlite3 *db)
{
	db::EqInserter inserter("equations", db);
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT rowid, math FROM phsp_targets", InsertTargetEquation, &inserter, &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			cerr << "failed to select phsp_targets: " << e << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

}

bool Form(sqlite3 *db)
{
	if (!BeginTransaction(db))
		return false;
	if (!CreateSingleton(db))
		return false;
	if (!CreateTable(db, "equations", "(uuid BLOB, body TEXT)"))
		return false;
	if (!InsertNames(db))
		return false;
	if (!InsertEquations(db))
		return false;
	return CommitTransaction(db);
}

}
}
