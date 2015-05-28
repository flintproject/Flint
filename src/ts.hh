/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TS_HH_
#define FLINT_TS_HH_

#include "runtime/timeseries.h"
#include "sqlite3.h"

namespace ts {

/*
 * This function may export ISDF files from CSV ones in the working directory.
 */
bool Tsc(sqlite3 *db);

/*
 * Note that db is for read only.
 */
bool LoadTimeseriesVector(sqlite3 *db, TimeseriesVector *tv);

}

#endif
