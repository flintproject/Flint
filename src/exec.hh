/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_HH_
#define FLINT_EXEC_HH_

#include "sqlite3.h"

namespace exec {

/*
 * Return true in case of success, false otherwise.
 */
bool Enum(sqlite3 *db);

}

#endif
