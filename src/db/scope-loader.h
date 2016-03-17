/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_SCOPE_LOADER_H_
#define FLINT_DB_SCOPE_LOADER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "statement-driver.h"

namespace flint {
namespace db {

class ScopeLoader : StatementDriver  {
public:
	// Note that db is for read only.
	explicit ScopeLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT * FROM scopes")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *uuid = sqlite3_column_blob(stmt(), 0);
			const void *space_id = sqlite3_column_blob(stmt(), 1);
			const unsigned char *label = sqlite3_column_text(stmt(), 2);
			assert(uuid);
			assert(space_id);
			boost::uuids::uuid uu, su;
			std::memcpy(&uu, uuid, uu.size());
			std::memcpy(&su, space_id, su.size());
			if (!handler->Handle(uu, su, (const char *)label))
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
