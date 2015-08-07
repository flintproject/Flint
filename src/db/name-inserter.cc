/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "name-inserter.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/nil_generator.hpp>

using std::cerr;
using std::endl;
using std::exit;
using std::printf;

namespace db {

NameInserter::NameInserter(const char *table, sqlite3 *db)
	: query_(new char[128]) // long enough
	, stmt_(nullptr)
{
	sprintf(query_.get(),
			"INSERT INTO %s VALUES (?, ?, ?, ?, 'dimensionless', NULL)",
			table);
	int e;
	e = sqlite3_prepare_v2(db, query_.get(), -1, &stmt_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << e
			 << ": " << query_.get()
			 << endl;
		exit(EXIT_FAILURE);
	}
}

NameInserter::~NameInserter()
{
	sqlite3_finalize(stmt_);
}

bool NameInserter::InsertName(const boost::uuids::uuid &space_id, char type, int id, const char *name)
{
	int e;
	e = sqlite3_bind_blob(stmt_, 1, &space_id, space_id.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind space_id: " << e << endl;
		return false;
	}
	e = sqlite3_bind_text(stmt_, 2, &type, 1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind type: " << e << endl;
		return false;
	}
	e = sqlite3_bind_int(stmt_, 3, id);
	if (e != SQLITE_OK) {
		cerr << "failed to bind id: " << e << endl;
		return false;
	}
	e = sqlite3_bind_text(stmt_, 4, name, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind name: " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt_);
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << e << endl;
		return false;
	}
	sqlite3_reset(stmt_);
	return true;
}

bool NameInserter::InsertName(char type, int id, const char *name)
{
	boost::uuids::uuid nu = boost::uuids::nil_uuid();
	return InsertName(nu, type, id, name);
}

}
