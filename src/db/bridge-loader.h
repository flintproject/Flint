/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_BRIDGE_LOADER_H_
#define FLINT_DB_BRIDGE_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/noncopyable.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include "sqlite3.h"

namespace db {

class BridgeLoader : boost::noncopyable {
public:
	explicit BridgeLoader(sqlite3 *db)
		: stmt_(NULL),
		  gen_()
	{
		int e = sqlite3_prepare_v2(db,
								   "SELECT m.module_id, p.pq_id, b.direction, b.sub_type, b.connector FROM bridges AS b LEFT JOIN pqs AS p ON b.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~BridgeLoader() {
		sqlite3_finalize(stmt_);
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *module_id = sqlite3_column_text(stmt_, 0);
			int pq_id = sqlite3_column_int(stmt_, 1);
			const unsigned char *direction = sqlite3_column_text(stmt_, 2);
			const unsigned char *sub_type = sqlite3_column_text(stmt_, 3);
			const unsigned char *connector = sqlite3_column_text(stmt_, 4);

			if (!handler->Handle(gen_((const char *)module_id), pq_id, (const char *)direction, (const char *)sub_type, (const char *)connector)) return false;
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
