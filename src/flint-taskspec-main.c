/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqlite3.h"

static void Usage(void)
{
	fprintf(stderr, "usage: flint-taskspec ID DB\n");
}

int main(int argc, char *argv[])
{
	int r = EXIT_FAILURE;

	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != 3) {
		Usage();
		return EXIT_FAILURE;
	}

	int id = atoi(argv[1]);
	if (id <= 0) {
		fprintf(stderr, "non-positive id: %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	sqlite3 *db;
	if (sqlite3_open(argv[2], &db) != SQLITE_OK) {
		fprintf(stderr, "failed to open database: %s\n", argv[2]);
		return EXIT_FAILURE;
	}
	int e;
	sqlite3_stmt *stmt;
	e = sqlite3_prepare_v2(db, "SELECT variable FROM dgs WHERE task_id = ?",
						   -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to prepare statement: %d\n", e);
		goto bail0;
	}
	e = sqlite3_bind_int64(stmt, 1, id);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to bind id: %d\n", e);
		goto bail1;
	}
	for (;;) {
		e = sqlite3_step(stmt);
		if (e == SQLITE_DONE) {
			r = EXIT_SUCCESS;
			goto bail1;
		}
		if (e != SQLITE_ROW) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			goto bail1;
		}
		printf("%s\n", sqlite3_column_text(stmt, 0));
	}

 bail1:
	sqlite3_finalize(stmt);
 bail0:
	sqlite3_close(db);
	return r;
}
