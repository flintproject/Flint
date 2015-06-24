/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_JOURNAL_LOADER_H_
#define FLINT_DB_JOURNAL_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/uuid_generators.hpp>

#include "statement-driver.hh"

namespace db {

class JournalLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit JournalLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT * FROM journals")
		, gen_()
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			int n = sqlite3_column_int(stmt(), 0);
			const unsigned char *uuid = sqlite3_column_text(stmt(), 1);
			if (!handler->Handle(n, gen_((const char *)uuid))) return false;
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
