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

namespace flint {

#ifdef HAVE_MKSTEMP

namespace {

char *MakeTemporaryFile(const std::string &name, int *fd)
{
	boost::system::error_code ec;
	boost::filesystem::path p = boost::filesystem::temp_directory_path(ec);
	if (ec) {
		std::cerr << "failed to get temporary directory path" << std::endl;
		return nullptr;
	}
	p /= name + ".XXXXXX";
	const char *pc = p.c_str();
	char *filepath = (char *)malloc(strlen(pc) + 1);
	if (!filepath) {
		std::cerr << "could not allocate filepath" << std::endl;
		return nullptr;
	}
	strcpy(filepath, pc);
	*fd = mkstemp(filepath);
	if (*fd < 0) {
		std::cerr << "could not make temporary file: " << filepath << std::endl;
		free(filepath);
		return nullptr;
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
	char *path = tempnam(nullptr, name_.c_str());
	if (!path) {
		std::cerr << "could not generate temporary name" << std::endl;
		return nullptr;
	}
	return path;
}

#endif

}
