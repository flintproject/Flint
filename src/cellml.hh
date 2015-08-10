/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CELLML_HH_
#define FLINT_CELLML_HH_

#include "sqlite3.h"

namespace flint {
namespace cellml {

/*
 * Read a CellML model.
 * Return true in case of success, false otherwise.
 */
bool Read(sqlite3 *db);

}
}

#endif
