/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "db.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "bc/index.h"
#include "statement-driver.h"
#include "query.h"

using std::memcpy;

namespace flint {
namespace db {

namespace {

class OffsetInserter : StatementDriver {
public:
	explicit OffsetInserter(sqlite3 *db)
		: StatementDriver(db, "INSERT INTO offsets VALUES (?, ?, ?, ?)")
		, offset_(kOffsetBase)
	{}

	int offset() const {return offset_;}

	bool Insert(boost::uuids::uuid u, const char *id, int size) {
		int e;
		e = sqlite3_bind_blob(stmt(), 1, &u, u.size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind uuid: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 2, id, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind id: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int(stmt(), 3, offset_++);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind value: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int(stmt(), 4, size);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind size: " << e << std::endl;
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

private:
	int offset_;
};

int InsertOffset(void *data, int argc, char **argv, char **names)
{
	OffsetInserter *inserter = static_cast<OffsetInserter *>(data);
	(void)names;
	assert(argc == 3);
	assert(argv[0]);
	assert(argv[1]);
	assert(argv[2]);
	boost::uuids::uuid u;
	memcpy(&u, argv[0], u.size());
	return inserter->Insert(u, argv[1], std::atoi(argv[2])) ? 0 : 1;
}

int CreateOffsets(sqlite3 *db)
{
	if (!CreateTable(db, "offsets",
					 "(uuid BLOB, id INTEGER, value INTEGER, size INTEGER)"))
		return false;

	OffsetInserter inserter(db);
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT sector_id, id, ncols * nrows FROM layout",
					 &InsertOffset, &inserter, &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			std::cerr << "failed to select layout: " << e << ": " << em << std::endl;
		sqlite3_free(em);
		return 0;
	}
	return inserter.offset();
}

}

bool Flow(sqlite3 *db)
{
	if (!BeginTransaction(db))
		return false;
	if (!CreateOffsets(db))
		return false;
	if (!CreateView(db, "flows",
					"SELECT s.value AS source, t.value AS target, r.reduction, s.size FROM reaches AS r"
					" LEFT JOIN offsets AS s ON r.output_uuid = s.uuid AND r.output_id = s.id"
					" LEFT JOIN offsets AS t ON r.input_uuid = t.uuid AND r.input_id = t.id"))
		return false;
	return CommitTransaction(db);
}

}
}
