/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_STATEMENT_DRIVER_HH_
#define FLINT_DB_STATEMENT_DRIVER_HH_

#include "sqlite3.h"

namespace flint {
namespace db {

class StatementDriver {
public:
	StatementDriver(const StatementDriver &) = delete;
	StatementDriver &operator=(const StatementDriver &) = delete;

	StatementDriver(sqlite3 *db, const char *query);

	~StatementDriver();

	sqlite3_stmt *stmt() const {return stmt_;}

private:
	sqlite3_stmt *stmt_;
};

}
}

#endif
