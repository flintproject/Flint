/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqlite3.h"

static sqlite3 *db;
static sqlite3_stmt *stmt;
static char *em;
static int e;

static const char DEFAULT_UUID[] = "00000000-0000-0000-0000-000000000000";

static int PrintParameter(void *data, int argc, char **argv, char **names)
{
	(void)data;

	for (int i=0;i<argc;i++) {
		printf("%s (eq %%%s %s)\n", DEFAULT_UUID, names[i], argv[i]);
	}
	return 0;
}

static int PrintEquation(void *data, int argc, char **argv, char **names)
{
	(void)data;
	(void)names;
	assert(argc == 2);

	printf("%s %s\n", argv[0], argv[1]);
	return 0;
}

static int Generate(int rowid, int enum_id)
{
	char query[1024];

	/* print equations */
	sprintf(query, "SELECT * FROM enum WHERE rowid = '%d'", enum_id);
	e = sqlite3_exec(db, query, PrintParameter, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to select enum: %s\n", em);
		return 1;
	}
	e = sqlite3_exec(db, "SELECT uuid, body FROM equations", PrintEquation, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to select equations: %s\n", em);
		return 1;
	}

	/* mark it generated */
	sprintf(query, "UPDATE jobs SET status = 'generated' WHERE rowid = '%d'", rowid);
	e = sqlite3_exec(db, query, NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to update jobs: %s\n", em);
		return EXIT_FAILURE;
	}

	return 0;
}

static void PrintUsage(const char *program)
{
	fprintf(stderr, "usage: %s DB\n", program);
}

int main(int argc, char *argv[])
{
	if (argc < 2) {
		PrintUsage(argv[0]);
		return EXIT_FAILURE;
	}
	if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
		PrintUsage(argv[0]);
		return EXIT_SUCCESS;
	}

	if (sqlite3_open(argv[1], &db) != SQLITE_OK) {
		fprintf(stderr, "could not open database: %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	e = sqlite3_prepare_v2(db, "SELECT rowid, enum_id FROM jobs WHERE status = 'pending' LIMIT 1", -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		/* TODO */
		return EXIT_FAILURE;
	}
	e = sqlite3_step(stmt);
	if (e != SQLITE_ROW) {
		fprintf(stderr, "no more pending row\n");
		/* TODO */
		return EXIT_FAILURE;
	}
	int rowid = sqlite3_column_int(stmt, 0);
	int enum_id = sqlite3_column_int(stmt, 1);
	sqlite3_finalize(stmt);

	/* start transaction */
	e = sqlite3_exec(db, "BEGIN", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to start transaction: %s\n", em);
		return EXIT_FAILURE;
	}

	Generate(rowid, enum_id);

	/* commit transaction */
	e = sqlite3_exec(db, "COMMIT", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to commit transaction: %s\n", em);
		/* TODO */
		return EXIT_FAILURE;
	}
	sqlite3_close(db);

	return EXIT_SUCCESS;
}
