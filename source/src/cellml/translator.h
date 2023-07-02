/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CELLML_TRANSLATOR_H_
#define FLINT_CELLML_TRANSLATOR_H_

#include "sqlite3.h"

namespace flint {

bool TranslateCellml(sqlite3 *db);

}

#endif
