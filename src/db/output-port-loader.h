/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_OUTPUT_PORT_LOADER_H_
#define FLINT_DB_OUTPUT_PORT_LOADER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "statement-driver.hh"

namespace flint {
namespace db {

class OutputPortLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit OutputPortLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT m.module_id, p.port_id, p.ref_pq_id, q.type FROM ports AS p LEFT JOIN modules AS m ON p.module_rowid = m.rowid LEFT JOIN pqs AS q ON p.ref_pq_id = q.pq_id AND m.rowid = q.module_rowid WHERE p.direction = 'out' AND p.ref_pq_id IS NOT NULL")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *module_id = sqlite3_column_blob(stmt(), 0);
			int port_id = sqlite3_column_int(stmt(), 1);
			int pq_id = sqlite3_column_int(stmt(), 2);
			const unsigned char *pq_type = sqlite3_column_text(stmt(), 3);
			assert(module_id);
			boost::uuids::uuid u;
			std::memcpy(&u, module_id, u.size());
			if (!handler->Handle(u, port_id, pq_id, *((const char *)pq_type)))
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
