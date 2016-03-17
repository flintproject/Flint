/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "driver.h"

#include <cstdio>
#include <cstdlib>
#include <iostream>

using std::cerr;
using std::endl;
using std::exit;

namespace flint {
namespace db {

Driver::Driver(const char *filename)
	: db_(nullptr)
{
	if (sqlite3_open(filename, &db_) != SQLITE_OK) {
		cerr << "failed to open database: " << filename << endl;
		exit(EXIT_FAILURE);
	}
}

Driver::~Driver()
{
	sqlite3_close(db_);
}

}
}
