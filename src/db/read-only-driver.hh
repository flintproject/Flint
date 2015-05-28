/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_READ_ONLY_DRIVER_HH_
#define FLINT_DB_READ_ONLY_DRIVER_HH_

#include <boost/noncopyable.hpp>

#include "sqlite3.h"

namespace db {

class ReadOnlyDriver : boost::noncopyable {
public:
	explicit ReadOnlyDriver(const char *filename);

	~ReadOnlyDriver();

	sqlite3 *db();

private:
	sqlite3 *db_;
};

}

#endif
