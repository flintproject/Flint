/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_STATEMENT_DRIVER_HH_
#define FLINT_DB_STATEMENT_DRIVER_HH_

#include <boost/noncopyable.hpp>

#include "sqlite3.h"

namespace db {

class StatementDriver : boost::noncopyable {
public:
	StatementDriver(sqlite3 *db, const char *query);

	~StatementDriver();

	sqlite3_stmt *stmt() {return stmt_;}

private:
	sqlite3_stmt *stmt_;
};

}

#endif
