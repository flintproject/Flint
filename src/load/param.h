/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LOAD_PARAM_H_
#define FLINT_LOAD_PARAM_H_

#include "sqlite3.h"

namespace flint {
namespace load {

/*
 * Note that db is for read only.
 * Return true in case of success, otherwise false.
 */
bool Param(sqlite3 *db, const char *output);

}
}

#endif
