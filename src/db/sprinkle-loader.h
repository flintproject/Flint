/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_SPRINKLE_LOADER_H_
#define FLINT_DB_SPRINKLE_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "statement-driver.h"

namespace db {

class SprinkleLoader : StatementDriver {
public:
	explicit SprinkleLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT * FROM sprinkles")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *track_id = sqlite3_column_blob(stmt(), 0);
			const void *sector_id = sqlite3_column_blob(stmt(), 1);
			int pq_id = sqlite3_column_int(stmt(), 2);
			double val = sqlite3_column_double(stmt(), 3);

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
		sqlite3_reset(stmt());
		return true;
	}
};

} // namespace db

#endif
