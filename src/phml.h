/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_H_
#define FLINT_PHML_H_

#include "sqlite3.h"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {
namespace phml {

/*
 * Return true in case of success, false otherwise.
 * Also, return given integer-valued seed as `seed' if requested.
 */
bool Nc(sqlite3 *db, const boost::filesystem::path &output, int *seed = nullptr);

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool UnitOfTime(sqlite3 *db, const boost::filesystem::path &output);

/*
 * Return true in case of success, false otherwise.
 */
bool LengthAndStep(sqlite3 *db, const boost::filesystem::path &nc_file, const boost::filesystem::path &uot_file);

bool CombineAll(sqlite3 *db, const boost::filesystem::path &dir);

bool Read(sqlite3 *db, const boost::filesystem::path &dir);

}
}

#endif
