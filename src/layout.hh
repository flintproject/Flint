/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LAYOUT_HH_
#define FLINT_LAYOUT_HH_

#include "sqlite3.h"

namespace flint {
namespace layout {

/*
 * Return true in case of success, otherwise false.
 */
bool Generate(sqlite3 *db, const char *filename);

}
}

#endif
