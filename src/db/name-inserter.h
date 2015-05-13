/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_NAME_INSERTER_H_
#define FLINT_DB_NAME_INSERTER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/noncopyable.hpp>
#include <boost/scoped_array.hpp>

#include "sqlite3.h"

namespace db {

class NameInserter : boost::noncopyable {
public:
	NameInserter(const char *table, sqlite3 *db)
		: query_(new char[128]) // long enough
		, stmt_(NULL)
	{
		std::sprintf(query_.get(),
					 "INSERT INTO %s VALUES (?, ?, ?, ?, 'dimensionless', NULL)",
					 table);
		int e;
		e = sqlite3_prepare_v2(db, query_.get(), -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e
					  << ": " << query_.get()
					  << std::endl;
			std::exit(EXIT_FAILURE);
		}
	}

	~NameInserter() {
		sqlite3_finalize(stmt_);
	}

	bool InsertName(const char *space_id, char type, int id, const char *name) {
		using std::cerr;
		using std::endl;

		int e;
		e = sqlite3_bind_text(stmt_, 1, space_id, -1, SQLITE_STATIC);
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

private:
	boost::scoped_array<char> query_;
	sqlite3_stmt *stmt_;
};

} // namespace db

#endif
