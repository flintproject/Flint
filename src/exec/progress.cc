/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/progress.hh"

#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fputc;
using std::fseek;
using std::perror;
using std::sprintf;
using std::strlen;

namespace flint {
namespace exec {

boost::interprocess::file_mapping *CreateProgressFile(int n, const char *dir)
{
	size_t len = strlen(dir);
	std::unique_ptr<char[]> filename(new char[len + 32]); // large enough
	sprintf(filename.get(), "%s/progress", dir);
	FILE *fp = fopen(filename.get(), "wb");
	if (!fp) {
		perror(filename.get());
		return nullptr;
	}
	int r = fseek(fp, n, SEEK_SET);
	if (r != 0) {
		cerr << "failed to seek: " << filename.get() << endl;
		fclose(fp);
		return nullptr;
	}
	if (fputc('\0', fp) == EOF) {
		cerr << "failed to write null character: " << filename.get() << endl;
		fclose(fp);
		return nullptr;
	}
	fclose(fp);
	return new boost::interprocess::file_mapping(filename.get(),
												 boost::interprocess::read_write);
}

}
}
