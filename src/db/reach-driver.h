/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_REACH_DRIVER_H_
#define FLINT_DB_REACH_DRIVER_H_

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "statement-driver.hh"

namespace db {

class ReachDriver : StatementDriver {
public:
	explicit ReachDriver(sqlite3 *db)
		: StatementDriver(db, "INSERT INTO reaches VALUES (?, ?, ?, ?)")
	{
	}

	bool Save(const boost::uuids::uuid &output_uuid, int output_id,
			  const boost::uuids::uuid &input_uuid, int input_id) {
		int e;
		e = sqlite3_bind_blob(stmt(), 1, &output_uuid, 16, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind output_uuid: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int(stmt(), 2, output_id);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind output_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_blob(stmt(), 3, &input_uuid, 16, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind input_uuid: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int(stmt(), 4, input_id);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind input_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

} // namespace db

#endif
