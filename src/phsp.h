/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHSP_H_
#define FLINT_PHSP_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace phsp {

/*
 * Read a PHSP model.
 * phsp_file is encoded in UTF-8.
 * Return true in case of success, false otherwise.
 */
bool Read(const char *phsp_file, sqlite3 *db, const boost::filesystem::path &dir);

}
}

#endif
