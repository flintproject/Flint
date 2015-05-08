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
		return 0;
	}
	return 1;
}
