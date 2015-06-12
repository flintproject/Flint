/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run/spec.hh"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>

using std::cerr;
using std::endl;

namespace run {

namespace {

int Print(void *data, int argc, char **argv, char **names)
{
	FILE *fp = static_cast<FILE *>(data);
	(void)names;
	assert(argc == 2);
	fprintf(fp, "%s %s\n", argv[0], argv[1]);
	return 0;
}

}

bool Spec(sqlite3 *db, FILE *fp)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT sector_id, name FROM layout WHERE type = 'v' OR type = 'x'",
					 &Print, fp, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to select layout: " << e
			 << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

}
