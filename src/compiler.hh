/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_HH_
#define FLINT_COMPILER_HH_

#include "sqlite3.h"

namespace flint {
namespace compiler {

/*
 * Note that db is for read only.
 */
bool Compile(sqlite3 *db, const char *table, const char *method, const char *output);

}
}

#endif
