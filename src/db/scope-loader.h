/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_SCOPE_LOADER_H_
#define FLINT_DB_SCOPE_LOADER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/uuid_generators.hpp>

#include "statement-driver.h"

namespace db {

class ScopeLoader : StatementDriver  {
public:
	explicit ScopeLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT * FROM scopes")
		, gen_()
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *uuid = sqlite3_column_text(stmt(), 0);
			const unsigned char *space_id = sqlite3_column_text(stmt(), 1);
			const unsigned char *label = sqlite3_column_text(stmt(), 2);
			if (!handler->Handle(gen_((const char *)uuid), gen_((const char *)space_id), (const char *)label)) return false;
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
