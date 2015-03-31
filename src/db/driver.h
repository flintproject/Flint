/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_DRIVER_H_
#define FLINT_DB_DRIVER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/noncopyable.hpp>

#include "sqlite3.h"

namespace db {

class Driver : boost::noncopyable {
public:
	explicit Driver(const char *filename)
		: db_(NULL)
	{
		if (sqlite3_open(filename, &db_) != SQLITE_OK) {
			std::cerr << "failed to open database: " << filename << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~Driver() {
		sqlite3_close(db_);
	}

	sqlite3 *db() {return db_;}

private:
	sqlite3 *db_;
};

} // namespace db

#endif
