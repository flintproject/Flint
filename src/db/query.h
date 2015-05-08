/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_QUERY_H_
#define FLINT_DB_QUERY_H_

#include "sqlite3.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Return 1 in case of success, 0 otherwise.
 */
int BeginTransaction(sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CommitTransaction(sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateTable(sqlite3 *db, const char *name, const char *columns);

#ifdef __cplusplus
}
#endif

#endif