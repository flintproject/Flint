/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_SPAN_LOADER_H_
#define FLINT_DB_SPAN_LOADER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "statement-driver.hh"

namespace flint {
namespace db {

class SpanLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit SpanLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT * FROM spans")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *tail_uuid = sqlite3_column_blob(stmt(), 0);
			int tail_port_id = sqlite3_column_int(stmt(), 1);
			const void *head_uuid = sqlite3_column_blob(stmt(), 2);
			int head_port_id = sqlite3_column_int(stmt(), 3);
			assert(tail_uuid);
			assert(head_uuid);
			boost::uuids::uuid tu, hu;
			std::memcpy(&tu, tail_uuid, tu.size());
			std::memcpy(&hu, head_uuid, hu.size());
			if (!handler->Handle(tu, tail_port_id, hu, head_port_id))
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
