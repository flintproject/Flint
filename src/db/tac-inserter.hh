/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_TAC_INSERTER_HH_
#define FLINT_DB_TAC_INSERTER_HH_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "statement-driver.h"

namespace db {

class TacInserter : StatementDriver {
public:
	explicit TacInserter(sqlite3 *db)
		: StatementDriver(db, "INSERT INTO tacs VALUES (?, ?, ?, ?)")
	{
	}

	bool Insert(const char *uuid, const char *name, int nod, const char *body) {
		using std::cerr;
		using std::endl;

		int e;
		e = sqlite3_bind_text(stmt(), 1, uuid, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 2, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind name: " << e << endl;
			return false;
		}
		e = sqlite3_bind_int(stmt(), 3, nod);
		if (e != SQLITE_OK) {
			cerr << "failed to bind nod: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 4, body, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind body: " << e << endl;
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
};

}

#endif
