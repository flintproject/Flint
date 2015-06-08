/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHSP_HH_
#define FLINT_PHSP_HH_

#include "sqlite3.h"

namespace phsp {

bool Read(sqlite3 *db);

}

#endif
