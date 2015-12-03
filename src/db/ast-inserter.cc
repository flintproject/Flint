/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "db/ast-inserter.h"

#include <cstdio>
#include <iostream>

using std::cerr;
using std::endl;

namespace flint {
namespace db {

AstInserter::AstInserter(sqlite3 *db)
	: StatementDriver(db, "INSERT INTO asts VALUES (?, ?, ?)")
{
}

AstInserter::~AstInserter() = default;

bool AstInserter::Insert(const boost::uuids::uuid &uuid,
						 const char *name,
						 const char *math)
{
	int e;
	e = sqlite3_bind_blob(stmt(), 1, &uuid, uuid.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind uuid: " << e << endl;
		return false;
	}
	e = sqlite3_bind_text(stmt(), 2, name, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind name: " << e << endl;
		return false;
	}
	e = sqlite3_bind_text(stmt(), 3, math, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind math: " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt());
	if (e != SQLITE_DONE) {
		cerr << "failed to step: " << e << endl;
		return false;
	}
	sqlite3_reset(stmt());
	return true;
}

}
}
