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

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateView(sqlite3 *db, const char *name, const char *query);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateSingleton(sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int SaveNol(int nol, sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateAsts(sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateLayout(sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateSprinkles(sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateTsfiles(sqlite3 *db);

/*
 * Return 1 in case of success, 0 otherwise.
 */
int CreateConfig(sqlite3 *db);

#ifdef __cplusplus
}
#endif

#endif
