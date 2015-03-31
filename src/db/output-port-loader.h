/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_OUTPUT_PORT_LOADER_H_
#define FLINT_DB_OUTPUT_PORT_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/noncopyable.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include "sqlite3.h"

namespace db {

class OutputPortLoader : boost::noncopyable {
public:
	explicit OutputPortLoader(sqlite3 *db)
		: stmt_(NULL),
		  gen_()
	{
		int e = sqlite3_prepare_v2(db,
								   "SELECT m.module_id, p.port_id, p.ref_pq_id, q.type FROM ports AS p LEFT JOIN modules AS m ON p.module_rowid = m.rowid LEFT JOIN pqs AS q ON p.ref_pq_id = q.pq_id AND m.rowid = q.module_rowid WHERE p.direction = 'out' AND p.ref_pq_id IS NOT NULL",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~OutputPortLoader() {
		sqlite3_finalize(stmt_);
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *module_id = sqlite3_column_text(stmt_, 0);
			int port_id = sqlite3_column_int(stmt_, 1);
			int pq_id = sqlite3_column_int(stmt_, 2);
			const unsigned char *pq_type = sqlite3_column_text(stmt_, 3);
			if (!handler->Handle(gen_((const char *)module_id), port_id, pq_id, *((const char *)pq_type))) return false;
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
	boost::uuids::string_generator gen_;
};

} // namespace db

#endif
