/* -*- Mode: C+; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_UTILITY_H_
#define FLINT_DB_UTILITY_H_

#include "sqlite3.h"

namespace flint {
namespace db {

template<int N>
int PrepareStatement(sqlite3 *db, const char (&query)[N], sqlite3_stmt **stmt)
{
	return sqlite3_prepare_v2(db, query, N, stmt, nullptr);
}

}
}

#endif
