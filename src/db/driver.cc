/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "driver.h"

#include <cstdio>
#include <iostream>

#include "utf8path.h"

namespace flint {
namespace db {

std::unique_ptr<Driver> Driver::Create(const boost::filesystem::path &path)
{
	std::unique_ptr<char[]> path_u(GetUtf8FromPath(path));
	std::unique_ptr<Driver> driver(new Driver(path_u.get()));
	return driver;
}

Driver::Driver(const char *filename)
	: db_(nullptr)
{
	if (sqlite3_open(filename, &db_) != SQLITE_OK) {
		std::cerr << "failed to open database: " << filename << std::endl;
		sqlite3_close(db_);
		db_ = nullptr;
	}
}

Driver::~Driver()
{
	sqlite3_close(db_);
}

}
}
