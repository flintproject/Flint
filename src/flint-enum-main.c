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

struct Range {
	int num_values;
	double *values;
};

static int Enumerate(void)
{
	static char SQL[] = "SELECT name, range FROM phsp_parameters";

	/* get parameters as well as their values */
	e = sqlite3_prepare_v2(db, SQL, -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to parepare statement: %s\n", SQL);
		return 1;
	}

	size_t qlen = 1024;
	char *q = malloc(qlen);
	if (!q) {
		perror(NULL);
		return 1;
	}
	sprintf(q, "CREATE TABLE enum (");
	char *p = q;
	while (*p) p++; /* move to '\0' */

	int num_params = 0;
	struct Range *ranges = NULL;
	size_t size_ranges = 0;

	while ( (e = sqlite3_step(stmt)) != SQLITE_DONE) {
		if (e != SQLITE_ROW) return 1; /* TODO */

		const unsigned char *n, *r;
		n = sqlite3_column_text(stmt, 0);
		size_t nlen = strlen((const char *)n);
		if (nlen == 0) {
			fprintf(stderr, "empty parameter name\n");
			/* TODO */
			return 1;
		}
		r = sqlite3_column_text(stmt, 1);
		size_t rlen = strlen((const char *)r);
		if (rlen == 0) {
			fprintf(stderr, "empty value\n");
			/* TODO */
			return 1;
		}

		if ( (p - q) + nlen + 10 >= qlen) {
			qlen *= 2;
			char *qq = realloc(q, qlen);
			if (!qq) return 1; /* TODO */
			p = qq + (p - q);
			q = qq;
		}
		for (size_t i=0;i<nlen;i++) {
			*p++ = n[i];
		}
		memcpy(p, " REAL, ", 7);
		p += 7;

		int num_values = 1;
		for (size_t i=0;i<rlen;i++) {
			if (r[i] == ',') num_values++;
		}
		size_ranges += sizeof(struct Range);
		void *rr = realloc(ranges, size_ranges);
		if (!rr) {
			/* TODO */
			return 1;
		}
		ranges = rr;
		ranges[num_params].num_values = num_values;
		void *vv = calloc(num_values, sizeof(double));
		if (!vv) {
			/* TODO */
			return 1;
		}
		ranges[num_params].values = vv;
		const char *s = (const char *)r;
		char *t;
		for (int i=0;i<num_values;i++) {
			errno = 0;
			double d = strtod(s, &t);
			if (t == s) {
				fprintf(stderr, "failed to read a double value: %s\n", s);
				/* TODO */
				return 1;
			} else if (errno == ERANGE) {
				if (d == HUGE_VAL || d == -HUGE_VAL) {
					fprintf(stderr, "invalid range: overflow: %s\n", s);
				} else {
					fprintf(stderr, "invalid range: underflow: %s\n", s);
				}
				/* TODO */
				return 1;
			}
			ranges[num_params].values[i] = d;
			s = t+1;
		}
		num_params++;
	}

	if (num_params == 0) {
		/* TODO */
		return 0;
	}

	*(p-2) = ')';
	*(p-1) = '\0';

	e = sqlite3_exec(db, q, NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to create table: %s\n", em);
		return EXIT_FAILURE;
	}
	sqlite3_reset(stmt);
	sqlite3_finalize(stmt);

	/* insert rows */
	sprintf(q, "INSERT INTO enum VALUES (?");
	for (int i=1;i<num_params;i++) {
		strcat(q, ", ?");
	}
	strcat(q, ")");
	e = sqlite3_prepare_v2(db, q, -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to parepare statement: %s\n", q);
		/* TODO */
		return 1;
	}

	/* prepare to insert corresponding entries in jobs */
	sqlite3_stmt *job_stmt;
	sprintf(q, "INSERT INTO jobs VALUES (?, 'pending')");
	e = sqlite3_prepare_v2(db, q, -1, &job_stmt, NULL);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to prepare statement: %s\n", q);
		/* TODO */
		return 1;
	}

	int num_rows = 1;
	for (int i=0;i<num_params;i++) {
		num_rows *= ranges[i].num_values;
	}
	int *job_ids = calloc(num_rows, sizeof(int));
	if (!job_ids) {
		fprintf(stderr, "failed to calloc");
		/* TODO */
		return 1;
	}
	for (int i=0;i<num_rows;i++) {
		int d0 = 1;
		int d1 = 1;
		for (int k=0;k<num_params;k++) {
			d0 *= ranges[k].num_values;
			e = sqlite3_bind_double(stmt, k+1, ranges[k].values[(i%d0)/d1]);
			if (e != SQLITE_OK) {
				fprintf(stderr, "failed to bind parameter: %d\n", e);
				/* TODO */
				return 1;
			}
			d1 *= ranges[k].num_values;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to insert row: %d\n", e);
			/* TODO */
			return 1;
		}
		sqlite3_int64 rowid = sqlite3_last_insert_rowid(db);
		e = sqlite3_bind_int64(job_stmt, 1, rowid);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind parameter: %d\n", e);
			/* TODO */
			return 1;
		}
		e = sqlite3_step(job_stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to insert row: %d\n", e);
			/* TODO */
			return 1;
		}
		int job_id = (int)sqlite3_last_insert_rowid(db); /* TODO */
		job_ids[i] = job_id;
		sqlite3_reset(stmt);
		sqlite3_reset(job_stmt);
	}
	sqlite3_finalize(stmt);
	sqlite3_finalize(job_stmt);

	printf("JOBS =");
	for (int i=0;i<num_rows;i++) {
		printf(" %d", job_ids[i]);
	}
	printf("\n");
	printf("\n");
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

	if (!CreateTable(db, "jobs", "(enum_id INTEGER, status TEXT)"))
		return EXIT_FAILURE;

	e = sqlite3_exec(db, "BEGIN", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to start transaction: %s\n", em);
		return EXIT_FAILURE;
	}

	if (Enumerate() != 0) return EXIT_FAILURE;

	e = sqlite3_exec(db, "COMMIT", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to commit transaction: %s\n", em);
		return EXIT_FAILURE;
	}
	sqlite3_close(db);

	return EXIT_SUCCESS;
}
