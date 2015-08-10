/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "task.hh"

#include <cstdlib>
#include <cstring>

using std::fprintf;

namespace flint {
namespace task {

bool Spec(int id, sqlite3 *db, FILE *fp)
{
	int r = false;

	int e;
	sqlite3_stmt *stmt;
	e = sqlite3_prepare_v2(db, "SELECT variable FROM dgs WHERE task_id = ?",
						   -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to prepare statement: %d\n", e);
		return false;
	}
	e = sqlite3_bind_int64(stmt, 1, id);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to bind id: %d\n", e);
		goto bail;
	}
	for (;;) {
		e = sqlite3_step(stmt);
		if (e == SQLITE_DONE) {
			r = true;
			goto bail;
		}
		if (e != SQLITE_ROW) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			goto bail;
		}
		fprintf(fp, "%s\n", sqlite3_column_text(stmt, 0));
	}

 bail:
	sqlite3_finalize(stmt);
	return r;
}

}
}
