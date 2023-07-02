/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_MODELPATH_H_
#define FLINT_MODELPATH_H_

#include "sqlite3.h"

namespace flint {

/*
 * Read the input file, and return an internal C string, which the client
 * code is responsible for freeing by delete [] after use.
 * Returns null in case of errors.
 * Note that db is for read only.
 */
char *GetGivenFilename(sqlite3 *db);
char *GetModelFilename(sqlite3 *db);

}

#endif
