/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run/spec.hh"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include <boost/uuid/uuid_io.hpp>

using std::cerr;
using std::endl;

namespace flint {
namespace run {

namespace {

int Print(void *data, int argc, char **argv, char **names)
{
	FILE *fp = static_cast<FILE *>(data);
	(void)names;
	assert(argc == 2);
	assert(argv[0]);
	boost::uuids::uuid sector_id;
	memcpy(&sector_id, argv[0], sector_id.size());
	std::string s = boost::uuids::to_string(sector_id);
	fprintf(fp, "%s %s\n", s.c_str(), argv[1]);
	return 0;
}

}

bool Spec(sqlite3 *db, FILE *fp)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT sector_id, name FROM layout WHERE type = 'v' OR type = 'x'",
					 &Print, fp, &em);
	if (e == SQLITE_OK)
		return true;
	if (e != SQLITE_ABORT)
		cerr << "failed to select layout: " << e
			 << ": " << em << endl;
	sqlite3_free(em);
	return false;
}

}
}
