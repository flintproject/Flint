/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "query.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int BeginTransaction(sqlite3 *db)
{
	char *em;
	int e = sqlite3_exec(db, "BEGIN", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to begin transaction: %d: %s\n", e, em);
		sqlite3_free(em);
		return 0;
	}
	return 1;
}

int CommitTransaction(sqlite3 *db)
{
	char *em;
	int e = sqlite3_exec(db, "COMMIT", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to commit transaction: %d: %s\n", e, em);
		sqlite3_free(em);
		return 0;
	}
	return 1;
}

int CreateTable(sqlite3 *db, const char *name, const char *columns)
{
	size_t len = 32;
	len += strlen(name);
	len += strlen(columns);

	char *buf = malloc(len);
	sprintf(buf, "CREATE TABLE IF NOT EXISTS %s %s", name, columns);

	char *em;
	int e = sqlite3_exec(db, buf, NULL, NULL, &em);
	free(buf);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to create table %s: %d: %s\n",
				name, e, em);
		sqlite3_free(em);
		return 0;
	}
	return 1;
}

int CreateSingleton(sqlite3 *db)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "CREATE VIEW IF NOT EXISTS spaces AS SELECT '00000000-0000-0000-0000-000000000000', 'default'",
					 NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to create spaces: %d: %s\n", e, em);
		sqlite3_free(em);
		return 0;
	}
	e = sqlite3_exec(db, "CREATE VIEW IF NOT EXISTS scopes AS SELECT '00000000-0000-0000-0000-000000000000', '00000000-0000-0000-0000-000000000000'",
					 NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to create scopes: %d: %s\n", e, em);
		sqlite3_free(em);
		return 0;
	}
	if (!CreateTable(db, "names", "(space_id TEXT, type TEXT, id INTEGER, name TEXT, unit TEXT, capacity REAL)"))
		return 0;
	if (!CreateTable(db, "private_names", "(space_id TEXT, type TEXT, id INTEGER, name TEXT, unit TEXT, capacity REAL)"))
		return 0;
	if (!CreateTable(db, "time_unit", "(name TEXT)"))
		return 0;
	return 1;
}
