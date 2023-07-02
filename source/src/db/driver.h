/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_DRIVER_H_
#define FLINT_DB_DRIVER_H_

#include <memory>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace db {

class Driver {
public:
	Driver(const Driver &) = delete;
	Driver &operator=(const Driver &) = delete;

	static std::unique_ptr<Driver> Create(const boost::filesystem::path &path);

	explicit Driver(const char *filename);

	~Driver();

	sqlite3 *db() {return db_;}

private:
	sqlite3 *db_;
};

}
}

#endif
