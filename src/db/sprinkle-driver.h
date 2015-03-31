/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_SPRINKLE_DRIVER_H_
#define FLINT_DB_SPRINKLE_DRIVER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/noncopyable.hpp>
#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace db {

class SprinkleDriver : boost::noncopyable {
public:
	explicit SprinkleDriver(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "INSERT INTO sprinkles VALUES (?, ?, ?, ?)",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~SprinkleDriver() {
		sqlite3_finalize(stmt_);
	}

	bool Save(const boost::uuids::uuid &track_id,
			  const boost::uuids::uuid &sector_id,
			  int pq_id,
			  double val) {
		int e;
		e = sqlite3_bind_blob(stmt_, 1, &track_id, 16, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind track_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_blob(stmt_, 2, &sector_id, 16, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind sector_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int(stmt_, 3, pq_id);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind pq_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_double(stmt_, 4, val);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind val: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(stmt_);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt_);
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

} // namespace db

#endif
