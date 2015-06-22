/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_HH_
#define FLINT_EXEC_HH_

#include "sqlite3.h"

namespace exec {

/*
 * Return true in case of success, false otherwise.
 */
bool Enum(sqlite3 *db);

/*
 * Both sedml_file and phsp_file are encoded in UTF-8.
 * Return true in case of success, false otherwise.
 */
bool Exec(const char *sedml_file, const char *phsp_file);

}

#endif
