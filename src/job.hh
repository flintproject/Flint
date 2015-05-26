/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_JOB_HH_
#define FLINT_JOB_HH_

#include "sqlite3.h"

namespace job {

bool Generate(sqlite3 *input, sqlite3 *output);

}

#endif
