/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_EQ_INSERTER_H_
#define FLINT_DB_EQ_INSERTER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <iostream>

#include <boost/noncopyable.hpp>

#include "sqlite3.h"

namespace db {

class EqInserter : boost::noncopyable {
public:
	EqInserter(const char *table, sqlite3 *db)
		: query_(new char[128]) // long enough
		, stmt_(NULL)
	{
		std::sprintf(query_.get(),
					 "INSERT INTO %s VALUES (?, ?)",
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

	~EqInserter() {
		sqlite3_finalize(stmt_);
	}

	bool Insert(const char *uuid, const char *math) {
		using std::cerr;
		using std::endl;

		int e;
		e = sqlite3_bind_text(stmt_, 1, uuid, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt_, 2, math, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind math: " << e << endl;
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
	std::unique_ptr<char[]> query_;
	sqlite3_stmt *stmt_;
};

} // namespace db

#endif
