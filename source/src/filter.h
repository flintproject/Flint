/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_H_
#define FLINT_FILTER_H_

#include <cstdio>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace filter {

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Create(sqlite3 *db,
			const boost::filesystem::path &spec_file,
			const boost::filesystem::path &layout_file,
			const boost::filesystem::path &output_file);

/*
 * Return true in case of success, false otherwise.
 */
bool Track(const boost::filesystem::path &filter_file,
		   const boost::filesystem::path &output_file);

/*
 * Return true in case of success, false otherwise.
 */
bool Isdh(const boost::filesystem::path &filter_file,
		  const boost::filesystem::path &output_file);

}
}

#endif
