/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "statement-driver.hh"

#include <cstdio>
#include <cstdlib>
#include <iostream>

using std::cerr;
using std::endl;

namespace db {

StatementDriver::StatementDriver(sqlite3 *db, const char *query)
	: stmt_(NULL)
{
	int e;
	e = sqlite3_prepare_v2(db, query, -1, &stmt_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << e
			 << ": " << query << endl;
		exit(EXIT_FAILURE);
	}
}

StatementDriver::~StatementDriver()
{
	sqlite3_finalize(stmt_);
}

}
