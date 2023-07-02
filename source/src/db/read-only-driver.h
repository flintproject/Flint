/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_READ_ONLY_DRIVER_H_
#define FLINT_DB_READ_ONLY_DRIVER_H_

#include <memory>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace db {

class ReadOnlyDriver {
public:
	ReadOnlyDriver(const ReadOnlyDriver &) = delete;
	ReadOnlyDriver &operator=(const ReadOnlyDriver &) = delete;

	static std::unique_ptr<ReadOnlyDriver> Create(const boost::filesystem::path &path);

	explicit ReadOnlyDriver(const char *filename);

	~ReadOnlyDriver();

	sqlite3 *db();

private:
	sqlite3 *db_;
};

}
}

#endif
