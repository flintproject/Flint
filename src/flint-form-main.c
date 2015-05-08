/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db/query.h"
#include "sqlite3.h"

static sqlite3 *db;
static sqlite3_stmt *stmt;
static char *em;
static int e;

static const char DEFAULT_UUID[] = "00000000-0000-0000-0000-000000000000";

static int InsertParameterName(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);

	int *id = (int *)data;
	e = sqlite3_bind_int(stmt, 1, *id);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	e = sqlite3_bind_text(stmt, 2, argv[0], -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	e = sqlite3_step(stmt);
	if (e != SQLITE_DONE) {
		/* TODO */
		return 1;
	}
	sqlite3_reset(stmt);
	printf("%s s %d %s\n", DEFAULT_UUID, *id, argv[0]);
	(*id)++;
	return 0;
}

static int InsertTargetName(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);

	int *id = (int *)data;
	int rowid = atoi(argv[0]);

	e = sqlite3_bind_int(stmt, 1, *id);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	char name[32];
	sprintf(name, "phsp:target%d", rowid);
	e = sqlite3_bind_text(stmt, 2, name, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	e = sqlite3_step(stmt);
	if (e != SQLITE_DONE) {
		/* TODO */
		return 1;
	}
	sqlite3_reset(stmt);
	printf("%s s %d %s\n", DEFAULT_UUID, *id, name);
	(*id)++;
	return 0;
}

static int InsertNames(void)
{
	int id = 1;
	char query[1024];
	sprintf(query, "INSERT INTO names VALUES ('%s', 's', ?, ?)", DEFAULT_UUID);
	e = sqlite3_prepare_v2(db, query, -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	e = sqlite3_exec(db, "SELECT name FROM phsp_parameters", InsertParameterName, &id, &em);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	e = sqlite3_exec(db, "SELECT rowid FROM phsp_targets", InsertTargetName, &id, &em);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	sqlite3_finalize(stmt);
	return 0;
}

static int InsertTargetEquation(void *data, int argc, char **argv, char **names)
{
	(void)data;
	(void)names;
	assert(argc == 2);

	int rowid = atoi(argv[0]);
	const char *math = argv[1];

	char *eqn = malloc(strlen(math) + 32);
	if (!eqn) {
		/* TODO */
		return 1;
	}
	sprintf(eqn, "(eq %%phsp:target%d%s)", rowid, math);
	e = sqlite3_bind_text(stmt, 1, eqn, -1, free);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	e = sqlite3_step(stmt);
	if (e != SQLITE_DONE) {
		/* TODO */
		return 1;
	}
	sqlite3_reset(stmt);
	return 0;
}

static int InsertEquations(void)
{
	char query[1024];
	sprintf(query, "INSERT INTO equations VALUES ('%s', ?)", DEFAULT_UUID);
	e = sqlite3_prepare_v2(db, query, -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	e = sqlite3_exec(db, "SELECT rowid, math FROM phsp_targets", InsertTargetEquation, NULL, &em);
	if (e != SQLITE_OK) {
		/* TODO */
		return 1;
	}
	sqlite3_finalize(stmt);
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

	/* start transaction */
	e = sqlite3_exec(db, "BEGIN", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to start transaction: %s\n", em);
		return EXIT_FAILURE;
	}

	if (!CreateTable(db, "modules", "(uuid TEXT, name TEXT)"))
		return EXIT_FAILURE;
	if (!CreateTable(db, "names", "(uuid TEXT, type TEXT, id INTEGER, name TEXT)"))
		return EXIT_FAILURE;
	if (!CreateTable(db, "edges", "(source_uuid TEXT, source_id INTEGER, target_uuid TEXT, target_id INTEGER)"))
		return EXIT_FAILURE;
	if (!CreateTable(db, "equations", "(uuid TEXT, body TEXT)"))
		return EXIT_FAILURE;

	/* insert the default module */
	e = sqlite3_exec(db, "INSERT INTO modules VALUES ('00000000-0000-0000-0000-000000000000', 'default')", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		/* TODO */
		return EXIT_FAILURE;
	}
	if (InsertNames() != 0) {
		return EXIT_FAILURE;
	}
	if (InsertEquations() != 0) {
		return EXIT_FAILURE;
	}

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
