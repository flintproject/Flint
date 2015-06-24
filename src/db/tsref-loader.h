/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_TSREF_LOADER_H_
#define FLINT_DB_TSREF_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/uuid_generators.hpp>

#include "statement-driver.hh"

namespace db {

class TsrefLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit TsrefLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT m.module_id, p.pq_id, r.timeseries_id, r.element_id FROM refts AS r LEFT JOIN pqs AS p ON r.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid")
		, gen_()
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *module_id = sqlite3_column_text(stmt(), 0);
			int pq_id = sqlite3_column_int(stmt(), 1);
			int ts_id = sqlite3_column_int(stmt(), 2);
			const unsigned char *element_id = sqlite3_column_text(stmt(), 3);
			if (!handler->Handle(gen_((const char *)module_id), pq_id, ts_id, (const char *)element_id)) return false;
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
