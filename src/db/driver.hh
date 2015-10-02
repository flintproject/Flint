/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_DRIVER_HH_
#define FLINT_DB_DRIVER_HH_

#include "sqlite3.h"

namespace flint {
namespace db {

class Driver {
public:
	Driver(const Driver &) = delete;
	Driver &operator=(const Driver &) = delete;

	explicit Driver(const char *filename);

	~Driver();

	sqlite3 *db() {return db_;}

private:
	sqlite3 *db_;
};

}
}

#endif
