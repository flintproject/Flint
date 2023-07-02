/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "variable-inserter.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

#include <boost/uuid/nil_generator.hpp>

namespace flint {
namespace db {

VariableInserter::VariableInserter(const char *table, bool independent, sqlite3 *db)
	: stmt_(nullptr)
	, independent_(independent)
{
	std::unique_ptr<char[]> query(new char[128]); // long enough
	int n_bytes = std::sprintf(query.get(),
			"INSERT INTO %s VALUES (?, ?, ?, ?, 'dimensionless', ?, ?, NULL, ?)",
			table);
	assert(n_bytes > 0);
	int e = sqlite3_prepare_v2(db, query.get(), n_bytes+1, &stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e
			 << ": " << query.get()
			 << std::endl;
	}
}

VariableInserter::~VariableInserter()
{
	sqlite3_finalize(stmt_);
}

bool VariableInserter::Insert(const boost::uuids::uuid &space_id,
							  char type,
							  int id,
							  const char *name,
							  int col,
							  int row)
{
	int e;
	e = sqlite3_bind_blob(stmt_, 1, &space_id, space_id.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind space_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(stmt_, 2, &type, 1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind type: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(stmt_, 3, id);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(stmt_, 4, name, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind name: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(stmt_, 5, col);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind ncols: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(stmt_, 6, row);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind nrows: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(stmt_, 7, independent_ ? 1 : 0);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind independent: " << e << std::endl;
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

bool VariableInserter::Insert(char type,
							  int id,
							  const char *name,
							  int col,
							  int row)
{
	boost::uuids::uuid nu = boost::uuids::nil_uuid();
	return Insert(nu, type, id, name, col, row);
}

}
}
