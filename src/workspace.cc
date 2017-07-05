/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/workspace.h"

#include <cassert>
#include <cstdio>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>

namespace flint {
namespace workspace {

bool CreateSparseFile(const boost::filesystem::path &filename, size_t size)
{
	boost::filesystem::ofstream ofs(filename, std::ios::out|std::ios::binary);
	if (!ofs) {
		std::cerr << "failed to open " << filename << std::endl;
		return false;
	}
	if (!ofs.seekp(size-1, std::ios::cur)) {
		std::cerr << "failed to seek: " << filename << std::endl;
		ofs.close();
		return false;
	}
	if (!ofs.put('\0')) {
		std::cerr << "failed to write null character: " << filename << std::endl;
		ofs.close();
		return false;
	}
	ofs.close();
	return bool(ofs);
}

bool CreateSparseFileAtomically(const boost::filesystem::path &filename, size_t size)
{
	boost::filesystem::path tmp_filename = filename;
	tmp_filename += ".tmp";
	if (!CreateSparseFile(tmp_filename, size))
		return false;
	boost::system::error_code ec;
	boost::filesystem::rename(tmp_filename, filename, ec);
	if (ec) {
		std::cerr << "failed to rename " << tmp_filename
				  << " to " << filename
				  << std::endl;
		return false;
	}
	return true;
}

}
}
