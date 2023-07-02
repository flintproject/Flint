/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_PARAMETER_H_
#define FLINT_EXEC_PARAMETER_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace exec {

/*
 * Save parameter names in the file "parameters.txt".
 * Return true in case of success, false otherwise.
 */
bool SaveParameters(const boost::filesystem::path &dir, sqlite3 *db);

}
}

#endif
