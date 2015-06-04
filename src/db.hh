/* -*- Mode: C+; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_HH_
#define FLINT_DB_HH_

#include "sqlite3.h"

namespace db {

bool Flow(sqlite3 *db);

}

#endif
