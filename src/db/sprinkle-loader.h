/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_SPRINKLE_LOADER_H_
#define FLINT_DB_SPRINKLE_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/noncopyable.hpp>
#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace db {

class SprinkleLoader : boost::noncopyable {
public:
	explicit SprinkleLoader(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db,
								   "SELECT * FROM sprinkles",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~SprinkleLoader() {
		sqlite3_finalize(stmt_);
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const void *track_id = sqlite3_column_blob(stmt_, 0);
			const void *sector_id = sqlite3_column_blob(stmt_, 1);
			int pq_id = sqlite3_column_int(stmt_, 2);
			double val = sqlite3_column_double(stmt_, 3);

			boost::uuids::uuid tu;
			std::memcpy(&tu, track_id, 16);
			boost::uuids::uuid su;
			std::memcpy(&su, sector_id, 16);
			if (!handler->Handle(tu, su, pq_id, val)) return false;
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
