/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SBML_HH_
#define FLINT_SBML_HH_

#include "sqlite3.h"

namespace flint {
namespace sbml {

bool Parse(const char *db_file);

bool Load(sqlite3 *db);

}
}

#endif
