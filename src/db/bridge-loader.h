/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_BRIDGE_LOADER_H_
#define FLINT_DB_BRIDGE_LOADER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "statement-driver.hh"

namespace flint {
namespace db {

class BridgeLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit BridgeLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT m.module_id, p.pq_id, b.direction, b.sub_type, b.connector FROM bridges AS b LEFT JOIN pqs AS p ON b.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *module_id = sqlite3_column_blob(stmt(), 0);
			int pq_id = sqlite3_column_int(stmt(), 1);
			const unsigned char *direction = sqlite3_column_text(stmt(), 2);
			const unsigned char *sub_type = sqlite3_column_text(stmt(), 3);
			const unsigned char *connector = sqlite3_column_text(stmt(), 4);
			assert(module_id);
			boost::uuids::uuid u;
			std::memcpy(&u, module_id, u.size());
			if (!handler->Handle(u, pq_id, (const char *)direction, (const char *)sub_type, (const char *)connector))
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
