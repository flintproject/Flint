/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_NAME_LOADER_H_
#define FLINT_DB_NAME_LOADER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/noncopyable.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include "sqlite3.h"

namespace db {

class NameLoader : boost::noncopyable {
public:
	explicit NameLoader(sqlite3 *db)
		: stmt_(NULL),
		  gen_()
	{
		int e = sqlite3_prepare_v2(db, "SELECT * FROM names UNION ALL SELECT * FROM private_names",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~NameLoader() {
		sqlite3_finalize(stmt_);
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *space_id = sqlite3_column_text(stmt_, 0);
			const unsigned char *type = sqlite3_column_text(stmt_, 1);
			int id = sqlite3_column_int(stmt_, 2);
			const unsigned char *name = sqlite3_column_text(stmt_, 3);
			const unsigned char *unit = sqlite3_column_text(stmt_, 4);
			double capacity = sqlite3_column_double(stmt_, 5);

			assert(space_id);
			if (!name) {
				std::cerr << "name for "
						  << space_id
						  << ":"
						  << id
						  << " is not found"
						  << std::endl;
				return false;
			}
			if (!unit) {
				std::cerr << "unit for "
						  << space_id
						  << ":"
						  << name
						  << " is not found"
						  << std::endl;
				return false;
			}
			if (!handler->Handle(gen_((const char *)space_id), (char)type[0], id, (const char *)name, (const char *)unit, capacity)) return false;
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
