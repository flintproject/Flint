/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "sys/temporary_path.h"

#include <cstdio>
#include <cstdlib>
#include <iostream>
#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

using std::cerr;
using std::endl;

namespace flint {

#ifdef HAVE_MKSTEMP

namespace {

char *MakeTemporaryFile(const std::string &name, int *fd)
{
	boost::system::error_code ec;
	boost::filesystem::path p = boost::filesystem::temp_directory_path(ec);
	if (ec) {
		cerr << "failed to get temporary directory path" << endl;
		return NULL;
	}
	p /= name + ".XXXXXX";
	const char *pc = p.c_str();
	char *filepath = (char *)malloc(strlen(pc) + 1);
	if (!filepath) {
		cerr << "could not allocate filepath" << endl;
		return NULL;
	}
	strcpy(filepath, pc);
	*fd = mkstemp(filepath);
	if (*fd < 0) {
		cerr << "could not make temporary file: " << filepath << endl;
		free(filepath);
		return NULL;
	}
	return filepath;
}

} // namespace

char *TemporaryPath::Touch()
{
	int fd;
	char *path = MakeTemporaryFile(name_, &fd);
	if (path) close(fd);
	return path;
}

#else

char *TemporaryPath::Touch()
{
	char *path = tempnam(NULL, name_.c_str());
	if (!path) {
		cerr << "could not generate temporary name" << endl;
		return NULL;
	}
	return path;
}

#endif

}
