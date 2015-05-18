/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_INPUT_PORT_LOADER_H_
#define FLINT_DB_INPUT_PORT_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/uuid_generators.hpp>

#include "statement-driver.h"

namespace db {

class InputPortLoader : StatementDriver {
public:
	explicit InputPortLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT m.module_id, p.pq_id, p.type, r.port_id FROM refports AS r LEFT JOIN pqs AS p ON r.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid")
		, gen_()
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *module_id = sqlite3_column_text(stmt(), 0);
			int pq_id = sqlite3_column_int(stmt(), 1);
			const unsigned char *pq_type = sqlite3_column_text(stmt(), 2);
			int port_id = sqlite3_column_int(stmt(), 3);
			if (!handler->Handle(gen_((const char *)module_id), port_id, pq_id, *((const char *)pq_type))) return false;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}

private:
	boost::uuids::string_generator gen_;
};

} // namespace db

#endif
