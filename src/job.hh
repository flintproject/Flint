/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_JOB_HH_
#define FLINT_JOB_HH_

#include "sqlite3.h"

namespace job {

bool Generate(sqlite3 *input, sqlite3 *output);

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Store(sqlite3 *db,
		   const char *source_layout_file, const char *source_data_file,
		   const char *target_layout_file, const char *target_data_file);

}

#endif
