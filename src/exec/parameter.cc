/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/parameter.hh"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::perror;
using std::sprintf;

namespace flint {
namespace exec {

namespace {

int WriteParameter(void *data, int argc, char **argv, char **names)
{
	boost::filesystem::ofstream *ofs = static_cast<boost::filesystem::ofstream *>(data);
	assert(ofs != nullptr);
	(void)names;
	assert(argc == 2);
	assert(argv[0] != nullptr);
	assert(argv[1] != nullptr);
	*ofs << argv[0] << '|' << argv[1] << endl;
	return 0;
}

}

bool SaveParameters(int id, sqlite3 *db)
{
	boost::system::error_code ec;
	char filename[64]; // long enough
	sprintf(filename, "%d/parameters.txt.tmp", id);
	boost::filesystem::path path(filename);
	boost::filesystem::ofstream ofs(path);
	if (!ofs.is_open()) {
		cerr << "failed to open " << path << endl;
		return false;
	}
	char *em;
	int e = sqlite3_exec(db, "SELECT name, range FROM phsp_parameters", &WriteParameter,
						 &ofs, &em);
	ofs.close();
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			cerr << "failed to select phsp_parameters: " << e << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}

	// rename the file to parameters.txt
	sprintf(filename, "%d/parameters.txt", id);
	boost::filesystem::rename(path, filename, ec);
	if (ec) {
		cerr << "failed to rename " << path
			 << " to " << filename
			 << endl;
		boost::filesystem::remove(path);
		return false;
	}
	return true;
}

}
}
