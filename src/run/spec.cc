/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run/spec.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <string>

#include <boost/uuid/uuid_io.hpp>

namespace flint {
namespace run {

namespace {

int Print(void *data, int argc, char **argv, char **names)
{
	std::ostream *os = static_cast<std::ostream *>(data);
	(void)names;
	assert(argc == 2);
	assert(argv[0]);
	boost::uuids::uuid sector_id;
	memcpy(&sector_id, argv[0], sector_id.size());
	std::string s = boost::uuids::to_string(sector_id);
	*os << s << ' ' << argv[1] << std::endl;
	return 0;
}

}

bool Spec(sqlite3 *db, std::ostream *os)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT sector_id, name FROM layout WHERE type = 'v' OR type = 'x'",
					 &Print, os, &em);
	if (e == SQLITE_OK)
		return true;
	if (e != SQLITE_ABORT)
		std::cerr << "failed to select layout: " << e
			 << ": " << em << std::endl;
	sqlite3_free(em);
	return false;
}

}
}
