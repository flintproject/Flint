/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_BRANCH_H_
#define FLINT_PHML_BRANCH_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace phml {

bool Branch(const boost::filesystem::path &path, sqlite3 *db);

}
}

#endif
