/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SBML_H_
#define FLINT_SBML_H_

#include "sqlite3.h"

namespace flint {
namespace sbml {

bool Parse(sqlite3 *db);

bool Load(sqlite3 *db);

/*
 * Read an SBML model.
 * Return true in case of success, false otherwise.
 */
bool Read(sqlite3 *db);

}
}

#endif
