/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TASK_HH_
#define FLINT_TASK_HH_

#include <cstdio>
#include "sqlite3.h"

namespace task {

/*
 * Print the given task's preference.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Pref(int id, sqlite3 *db, FILE *fp);

/*
 * List a task's spec.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Spec(int id, sqlite3 *db, FILE *fp);

/*
 * Return true in case of success, false otherwise.
 */
bool Form(sqlite3 *db);

}

#endif
