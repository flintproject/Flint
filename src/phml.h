/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_H_
#define FLINT_PHML_H_

#include "sqlite3.h"

namespace flint {
namespace phml {

/*
 * Return true in case of success, false otherwise.
 * Also, return given integer-valued seed as `seed' if requested.
 */
bool Nc(sqlite3 *db, const char *output, int *seed = nullptr);

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool UnitOfTime(sqlite3 *db, const char *output);

/*
 * Return true in case of success, false otherwise.
 */
bool LengthAndStep(sqlite3 *db, const char *nc_file, const char *uot_file);

bool CombineAll(sqlite3 *db);

bool Read(sqlite3 *db);

}
}

#endif
