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

char *MakeTemporaryFile(const std::string &name, const std::string &dir, int *fd)
{
	using namespace boost::filesystem;

	path p(dir.empty() ? "/tmp" : dir);
	const char *tmpdir = getenv("TMPDIR");
	if (tmpdir && strlen(tmpdir) > 0) {
		p = path(tmpdir);
	}
	if (!exists(p) || !is_directory(p)) {
		cerr << p << " is not a proper directory" << endl;
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
	char *path = MakeTemporaryFile(name_, directory_, &fd);
	if (path) close(fd);
	return path;
}

#else

char *TemporaryPath::Touch()
{
	char *path = tempnam(directory_.empty() ? NULL : directory_.c_str(), name_.c_str());
	if (!path) {
		cerr << "could not generate temporary name" << endl;
		return NULL;
	}
	return path;
}

#endif

}
