/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "statement-driver.h"

#include <cassert>

namespace flint {
namespace db {

StatementDriver::StatementDriver(sqlite3_stmt *stmt)
	: stmt_(stmt)
{
	assert(stmt);
}

StatementDriver::~StatementDriver()
{
	sqlite3_finalize(stmt_);
}

}
}
