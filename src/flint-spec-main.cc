/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "db/query.h"
#include "db/read-only-driver.hh"

using std::cerr;
using std::endl;

namespace {

int Print(void *data, int argc, char **argv, char **names)
{
	(void)data;
	(void)names;
	assert(argc == 2);
	std::printf("%s %s\n", argv[0], argv[1]);
	return 0;
}

bool Process(sqlite3 *db)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT sector_id, name FROM layout WHERE type = 'v' OR type = 'x'",
					 &Print, NULL, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to select layout: " << e
			 << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

void Usage()
{
	cerr << "usage: flint-spec DB" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if ( std::strcmp(argv[1], "-h") == 0 ||
		 std::strcmp(argv[1], "--help") == 0 ) {
		Usage();
		return EXIT_SUCCESS;
	}
	db::ReadOnlyDriver driver(argv[1]);
	return Process(driver.db()) ? EXIT_SUCCESS : EXIT_FAILURE;
}
