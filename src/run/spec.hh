/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUN_SPEC_HH_
#define FLINT_RUN_SPEC_HH_

#include <cstdio>
#include "sqlite3.h"

namespace flint {
namespace run {

/*
 * Return true in case of success, otherwise false.
 */
bool Spec(sqlite3 *db, FILE *fp);

}
}

#endif
