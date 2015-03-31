/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_REACH_LOADER_H_
#define FLINT_DB_REACH_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/noncopyable.hpp>

#include "sqlite3.h"

namespace db {

class ReachLoader : boost::noncopyable {
public:
	explicit ReachLoader(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "SELECT * FROM reaches",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~ReachLoader() {
		sqlite3_finalize(stmt_);
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const void *output_uuid = sqlite3_column_blob(stmt_, 0);
			int output_id = sqlite3_column_int(stmt_, 1);
			const void *input_uuid = sqlite3_column_blob(stmt_, 2);
			int input_id = sqlite3_column_int(stmt_, 3);
			if (!handler->Handle(output_uuid, output_id, input_uuid, input_id)) return false;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt_);
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

} // namespace db

#endif
