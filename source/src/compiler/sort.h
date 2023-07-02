/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_SORT_H_
#define FLINT_COMPILER_SORT_H_

#include "sqlite3.h"

namespace flint {
namespace compiler {
namespace sort {

bool Sort(sqlite3 *db);

}
}
}

#endif
