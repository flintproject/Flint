/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHZ_H_
#define FLINT_PHZ_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace phz {

/*
 * Read a PHZ file.
 * Return true in case of success, false otherwise.
 */
bool Read(sqlite3 *db, const boost::filesystem::path &dir);

}
}

#endif
