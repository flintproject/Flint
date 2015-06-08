/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SEDML_HH_
#define FLINT_SEDML_HH_

#include "sqlite3.h"

namespace sedml {

/*
 * Extract simulation configuration in SEDML.
 * Return true in case of success, false otherwise.
 */
bool Read(sqlite3 *db);

}

#endif
