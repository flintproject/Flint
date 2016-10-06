/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "statement-driver.h"

#include <cstdio>
#include <cstdlib>
#include <iostream>

namespace flint {
namespace db {

StatementDriver::StatementDriver(sqlite3 *db, const char *query)
	: stmt_(nullptr)
{
	int e;
	e = sqlite3_prepare_v2(db, query, -1, &stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e
			 << ": " << query << std::endl;
		exit(EXIT_FAILURE);
	}
}

StatementDriver::~StatementDriver()
{
	sqlite3_finalize(stmt_);
}

}
}
