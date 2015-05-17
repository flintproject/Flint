/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_STATEMENT_DRIVER_H_
#define FLINT_DB_STATEMENT_DRIVER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/noncopyable.hpp>

#include "sqlite3.h"

namespace db {

class StatementDriver : boost::noncopyable {
public:
	StatementDriver(sqlite3 *db, const char *query)
		: stmt_(NULL)
	{
		int e;
		e = sqlite3_prepare_v2(db, query, -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e
					  << ": " << query << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~StatementDriver() {
		sqlite3_finalize(stmt_);
	}

	sqlite3_stmt *stmt() {return stmt_;}

private:
	sqlite3_stmt *stmt_;
};

} // namespace db

#endif
