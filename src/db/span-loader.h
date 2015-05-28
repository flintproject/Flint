/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_SPAN_LOADER_H_
#define FLINT_DB_SPAN_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/uuid_generators.hpp>

#include "statement-driver.h"

namespace db {

class SpanLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit SpanLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT * FROM spans")
		, gen_()
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *tail_uuid = sqlite3_column_text(stmt(), 0);
			int tail_port_id = sqlite3_column_int(stmt(), 1);
			const unsigned char *head_uuid = sqlite3_column_text(stmt(), 2);
			int head_port_id = sqlite3_column_int(stmt(), 3);
			if (!handler->Handle(gen_((const char *)tail_uuid), tail_port_id, gen_((const char *)head_uuid), head_port_id)) return false;
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
