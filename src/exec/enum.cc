/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "exec.h"

#include <cassert>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "db/query.h"

using std::calloc;
using std::fprintf;
using std::malloc;
using std::memcpy;
using std::perror;
using std::printf;
using std::realloc;
using std::sprintf;
using std::strcat;
using std::strlen;

namespace flint {
namespace exec {

namespace {

struct Range {
	int num_values;
	double *values;
};

int Enumerate(sqlite3 *db)
{
	static char SQL[] = "SELECT name, range FROM phsp_parameters";

	sqlite3_stmt *stmt;
	char *em;
	int e;
	/* get parameters as well as their values */
	e = sqlite3_prepare_v2(db, SQL, -1, &stmt, nullptr);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to parepare statement: %s\n", SQL);
		return 0;
	}

	size_t qlen = 1024;
	char *q = static_cast<char *>(malloc(qlen));
	if (!q) {
		perror(nullptr);
		return 0;
	}
	sprintf(q, "CREATE TABLE enum (");
	char *p = q;
	while (*p) p++; /* move to '\0' */

	int num_params = 0;
	struct Range *ranges = nullptr;
	size_t size_ranges = 0;

	while ( (e = sqlite3_step(stmt)) != SQLITE_DONE) {
		if (e != SQLITE_ROW) return 0; /* TODO */

		const unsigned char *n, *r;
		n = sqlite3_column_text(stmt, 0);
		size_t nlen = strlen((const char *)n);
		if (nlen == 0) {
			fprintf(stderr, "empty parameter name\n");
			/* TODO */
			return 0;
		}
		r = sqlite3_column_text(stmt, 1);
		size_t rlen = strlen((const char *)r);
		if (rlen == 0) {
			fprintf(stderr, "empty value\n");
			/* TODO */
			return 0;
		}

		if ( (p - q) + nlen + 10 >= qlen) {
			qlen *= 2;
			char *qq = static_cast<char *>(realloc(q, qlen));
			if (!qq) return 0; /* TODO */
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
			return 0;
		}
		ranges = static_cast<Range *>(rr);
		ranges[num_params].num_values = num_values;
		void *vv = calloc(num_values, sizeof(double));
		if (!vv) {
			/* TODO */
			return 0;
		}
		ranges[num_params].values = static_cast<double *>(vv);
		const char *s = (const char *)r;
		char *t;
		for (int i=0;i<num_values;i++) {
			errno = 0;
			double d = strtod(s, &t);
			if (t == s) {
				fprintf(stderr, "failed to read a double value: %s\n", s);
				/* TODO */
				return 0;
			} else if (errno == ERANGE) {
				if (d == HUGE_VAL || d == -HUGE_VAL) {
					fprintf(stderr, "invalid range: overflow: %s\n", s);
				} else {
					fprintf(stderr, "invalid range: underflow: %s\n", s);
				}
				/* TODO */
				return 0;
			}
			ranges[num_params].values[i] = d;
			s = t+1;
		}
		num_params++;
	}

	if (num_params == 0) {
		/* TODO */
		return 1;
	}

	*(p-2) = ')';
	*(p-1) = '\0';

	e = sqlite3_exec(db, q, nullptr, nullptr, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to create table: %d: %s\n", e, em);
		sqlite3_free(em);
		return 0;
	}
	sqlite3_reset(stmt);
	sqlite3_finalize(stmt);

	/* insert rows */
	sprintf(q, "INSERT INTO enum VALUES (?");
	for (int i=1;i<num_params;i++) {
		strcat(q, ", ?");
	}
	strcat(q, ")");
	e = sqlite3_prepare_v2(db, q, -1, &stmt, nullptr);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to parepare statement: %s\n", q);
		/* TODO */
		return 0;
	}

	/* prepare to insert corresponding entries in jobs */
	sqlite3_stmt *job_stmt;
	sprintf(q, "INSERT INTO jobs VALUES (?, 'pending')");
	e = sqlite3_prepare_v2(db, q, -1, &job_stmt, nullptr);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to prepare statement: %s\n", q);
		/* TODO */
		return 0;
	}

	int num_rows = 1;
	for (int i=0;i<num_params;i++) {
		num_rows *= ranges[i].num_values;
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
				return 0;
			}
			d1 *= ranges[k].num_values;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to insert row: %d\n", e);
			/* TODO */
			return 0;
		}
		sqlite3_int64 rowid = sqlite3_last_insert_rowid(db);
		e = sqlite3_bind_int64(job_stmt, 1, rowid);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind parameter: %d\n", e);
			/* TODO */
			return 0;
		}
		e = sqlite3_step(job_stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to insert row: %d\n", e);
			/* TODO */
			return 0;
		}
		sqlite3_reset(stmt);
		sqlite3_reset(job_stmt);
	}
	sqlite3_finalize(stmt);
	sqlite3_finalize(job_stmt);
	return num_rows;
}

}

int Enum(sqlite3 *db)
{
	if (!CreateTable(db, "jobs", "(enum_id INTEGER, status TEXT)"))
		return false;
	if (!BeginTransaction(db))
		return false;
	int n = Enumerate(db);
	if (n == 0)
		return false;
	return CommitTransaction(db) ? n : 0;
}

}
}
