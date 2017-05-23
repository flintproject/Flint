/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/workspace.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

namespace flint {
namespace workspace {

bool CreateSparseFile(const char *filename, size_t size)
{
	FILE *fp = std::fopen(filename, "wb");
	if (!fp) {
		std::cerr << "failed to open " << filename << std::endl;
		return false;
	}
	int r = std::fseek(fp, size-1, SEEK_SET);
	if (r != 0) {
		std::cerr << "failed to seek: " << filename << std::endl;
		std::fclose(fp);
		return false;
	}
	if (std::fputc('\0', fp) == EOF) {
		std::cerr << "failed to write null character: " << filename << std::endl;
		std::fclose(fp);
		return false;
	}
	r = std::fclose(fp);
	if (r != 0) {
		std::cerr << "failed to close " << filename << std::endl;
		return false;
	}
	return true;
}

bool CreateSparseFileAtomically(const char *filename, size_t size)
{
	size_t len = std::strlen(filename);
	len += 4;
	assert(len > 0);
	std::unique_ptr<char[]> tmp_filename(new char[len+1]);
	int r = std::sprintf(tmp_filename.get(), "%s.tmp", filename);
	if (static_cast<size_t>(r) != len) {
		std::cerr << "failed to build temporary filename: " << filename << std::endl;
		return false;
	}
	if (!CreateSparseFile(tmp_filename.get(), size))
		return false;
	r = std::rename(tmp_filename.get(), filename);
	if (r != 0) {
		std::cerr << "failed to rename " << tmp_filename.get()
				  << " to " << filename
				  << std::endl;
		return false;
	}
	return true;
}

}
}
