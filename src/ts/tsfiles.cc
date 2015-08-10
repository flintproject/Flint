/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "ts.hh"

#include <cassert>
#include <cstdio>

#include "utf8path.h"

using std::cerr;
using std::endl;

namespace flint {
namespace ts {

namespace {

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);
	TimeseriesVector *tv = static_cast<TimeseriesVector *>(data);
	tv->push_back(new TimeseriesData(GetPathFromUtf8(argv[0])));
	return 0;
}

}

bool LoadTimeseriesVector(sqlite3 *db, TimeseriesVector *tv)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT * FROM tsfiles", &Process, tv, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to select tsfiles: " << e
			 << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	for (TimeseriesVector::iterator it=tv->begin();it!=tv->end();++it) {
		if (!it->Load()) return false;
	}
	return true;
}

}
}
