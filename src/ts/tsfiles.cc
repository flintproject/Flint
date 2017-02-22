/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "ts.h"

#include <cassert>
#include <cstdio>
#include <iostream>

#include "utf8path.h"

namespace flint {
namespace ts {

namespace {

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);
	TimeseriesVector *tv = static_cast<TimeseriesVector *>(data);
	boost::filesystem::path path = GetPathFromUtf8(argv[0]);
	if (path.empty())
		return 1;
	std::unique_ptr<TimeseriesData> td(new TimeseriesData(path));
	tv->push_back(std::move(td));
	return 0;
}

}

bool LoadTimeseriesVector(sqlite3 *db, TimeseriesVector *tv)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT * FROM tsfiles", &Process, tv, &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to select tsfiles: " << e
			 << ": " << em << std::endl;
		sqlite3_free(em);
		return false;
	}
	for (auto &td : *tv) {
		if (!td->Load()) return false;
	}
	return true;
}

}
}
