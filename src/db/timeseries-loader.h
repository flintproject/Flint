/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_TIMESERIES_LOADER_H_
#define FLINT_DB_TIMESERIES_LOADER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "statement-driver.hh"

namespace flint {
namespace db {

class TimeseriesLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit TimeseriesLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT m.module_id, t.timeseries_id, t.format, t.ref FROM timeseries AS t LEFT JOIN modules AS m ON t.module_rowid = m.rowid")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *module_id = sqlite3_column_blob(stmt(), 0);
			int ts_id = sqlite3_column_int(stmt(), 1);
			const unsigned char *format = sqlite3_column_text(stmt(), 2);
			const unsigned char *ref = sqlite3_column_text(stmt(), 3);
			assert(module_id);
			boost::uuids::uuid u;
			std::memcpy(&u, module_id, u.size());
			if (!handler->Handle(u, ts_id, (const char *)format, (const char *)ref))
				return false;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

}
}

#endif
