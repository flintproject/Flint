/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_REACH_LOADER_H_
#define FLINT_DB_REACH_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "statement-driver.h"

namespace db {

class ReachLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit ReachLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT * FROM reaches")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *output_uuid = sqlite3_column_blob(stmt(), 0);
			int output_id = sqlite3_column_int(stmt(), 1);
			const void *input_uuid = sqlite3_column_blob(stmt(), 2);
			int input_id = sqlite3_column_int(stmt(), 3);
			if (!handler->Handle(output_uuid, output_id, input_uuid, input_id)) return false;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

} // namespace db

#endif
