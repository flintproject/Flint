/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "tac-inserter.h"

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/nil_generator.hpp>

using std::cerr;
using std::endl;

namespace flint {
namespace db {

TacInserter::TacInserter(sqlite3 *db)
	: StatementDriver(db, "INSERT INTO tacs VALUES (?, ?, ?, ?, ?)")
{}

bool TacInserter::Insert(const boost::uuids::uuid &uuid, const char *name,
						 int noir, int nod, const char *body)
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
	e = sqlite3_bind_int(stmt(), 3, noir);
	if (e != SQLITE_OK) {
		cerr << "failed to bind noir: " << e << endl;
		return false;
	}
	e = sqlite3_bind_int(stmt(), 4, nod);
	if (e != SQLITE_OK) {
		cerr << "failed to bind nod: " << e << endl;
		return false;
	}
	e = sqlite3_bind_text(stmt(), 5, body, -1, SQLITE_STATIC);
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

bool TacInserter::Insert(const char *name, int noir, int nod, const char *body)
{
	boost::uuids::uuid nu = boost::uuids::nil_uuid();
	return Insert(nu, name, noir, nod, body);
}

}
}
