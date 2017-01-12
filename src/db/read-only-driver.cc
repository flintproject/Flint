/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "read-only-driver.h"

#include <cstdio>
#include <iostream>

namespace flint {
namespace db {

ReadOnlyDriver::ReadOnlyDriver(const char *filename)
	: db_(nullptr)
{
	if (sqlite3_open_v2(filename, &db_, SQLITE_OPEN_READONLY, nullptr) != SQLITE_OK) {
		std::cerr << "failed to open database for read-only: " << filename << std::endl;
		sqlite3_close(db_);
		db_ = nullptr;
	}
}

ReadOnlyDriver::~ReadOnlyDriver()
{
	sqlite3_close(db_);
}

sqlite3 *ReadOnlyDriver::db()
{
	return db_;
}

}
}
