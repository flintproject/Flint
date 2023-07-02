/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_COMBINE_H_
#define FLINT_PHML_COMBINE_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace flint {

bool Combine(const boost::uuids::uuid &uuid,
			 sqlite3 *db,
			 const boost::filesystem::path &dir);

}

#endif
