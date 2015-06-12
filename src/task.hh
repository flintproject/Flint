/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TASK_HH_
#define FLINT_TASK_HH_

#include <cstdio>
#include "sqlite3.h"

namespace task {

/*
 * Save a task's configuration into an attached database.
 * Return true in case of success, false otherwise.
 */
bool Config(int id, sqlite3 *db);

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

/*
 * Return true in case of success, false otherwise.
 */
bool Timer(double length, double step, FILE *fp);

}

#endif
