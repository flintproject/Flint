/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "db/driver.hh"
#include "db/name-inserter.h"
#include "db/query.h"

using std::cerr;
using std::endl;

int main(int argc, char *argv[])
{
	static const char kDefaultSpaceId[] = "00000000-0000-0000-0000-000000000000";

	assert(argc == 2);
	db::Driver driver(argv[1]);
	sqlite3 *db = driver.db();
	if (!BeginTransaction(db))
		return EXIT_FAILURE;
	if (!CreateSingleton(db))
		return EXIT_FAILURE;
	db::NameInserter inserter("names", db);
	if ( !inserter.InsertName(kDefaultSpaceId, 'v', 1, "a") ||
		 !inserter.InsertName(kDefaultSpaceId, 'v', 2, "b") ||
		 !inserter.InsertName(kDefaultSpaceId, 'v', 3, "x") )
		return EXIT_FAILURE;
	return CommitTransaction(db) ? EXIT_SUCCESS : EXIT_FAILURE;
}
