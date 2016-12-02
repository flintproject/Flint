/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_STATEMENT_DRIVER_H_
#define FLINT_DB_STATEMENT_DRIVER_H_

#include <cstdio>
#include <iostream>
#include "sqlite3.h"

namespace flint {
namespace db {

class StatementDriver {
public:
	StatementDriver(const StatementDriver &) = delete;
	StatementDriver &operator=(const StatementDriver &) = delete;

	template<int N>
	StatementDriver(sqlite3 *db, const char (&query)[N])
		: stmt_(nullptr)
	{
		int e = sqlite3_prepare_v2(db, query, N, &stmt_, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e
					  << ": " << query << std::endl;
		}
	}

	explicit StatementDriver(sqlite3_stmt *stmt);

	~StatementDriver();

	sqlite3_stmt *stmt() const {return stmt_;}

private:
	sqlite3_stmt *stmt_;
};

}
}

#endif
