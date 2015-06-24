/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "db.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/uuid/string_generator.hpp>

#include "bc/index.h"
#include "statement-driver.hh"
#include "query.h"

using std::cerr;
using std::endl;

namespace db {

namespace {

class OffsetInserter : StatementDriver {
public:
	explicit OffsetInserter(sqlite3 *db)
		: StatementDriver(db, "INSERT INTO offsets VALUES (?, ?, ?)")
		, offset_(kOffsetBase)
	{}

	int offset() const {return offset_;}

	bool Insert(boost::uuids::uuid u, const char *id) {
		int e;
		e = sqlite3_bind_blob(stmt(), 1, &u, 16, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 2, id, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind id: " << e << endl;
			return false;
		}
		e = sqlite3_bind_int(stmt(), 3, offset_++); // TODO: variable data size
		if (e != SQLITE_OK) {
			cerr << "failed to bind value: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
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
	static boost::uuids::string_generator gen;

	OffsetInserter *inserter = static_cast<OffsetInserter *>(data);
	(void)names;
	assert(argc == 2);
	assert(argv[0]);
	assert(argv[1]);
	return inserter->Insert(gen(argv[0]), argv[1]) ? 0 : 1;
}

int CreateOffsets(sqlite3 *db)
{
	if (!CreateTable(db, "offsets", "(uuid BLOB, id INTEGER, value INTEGER)"))
		return false;

	OffsetInserter inserter(db);
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT sector_id, id FROM layout",
					 &InsertOffset, &inserter, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to select layout: " << e
			 << ": " << em << endl;
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
					"SELECT s.value AS source, t.value AS target FROM reaches AS r"
					" LEFT JOIN offsets AS s ON r.output_uuid = s.uuid AND r.output_id = s.id"
					" LEFT JOIN offsets AS t ON r.input_uuid = t.uuid AND r.input_id = t.id"))
		return false;
	return CommitTransaction(db);
}

}
