/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_VARIABLE_INSERTER_H_
#define FLINT_DB_VARIABLE_INSERTER_H_

#include <memory>

#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace flint {
namespace db {

class VariableInserter {
public:
	VariableInserter(const VariableInserter &) = delete;
	VariableInserter &operator=(const VariableInserter &) = delete;

	VariableInserter(const char *table, sqlite3 *db);

	~VariableInserter();

	bool Insert(const boost::uuids::uuid &space_id, char type, int id, const char *name,
				int col = 1, int row = 1);

	/*
	 * Call Insert() with the default nil UUID.
	 */
	bool Insert(char type, int id, const char *name,
				int col = 1, int row = 1);

private:
	std::unique_ptr<char[]> query_;
	sqlite3_stmt *stmt_;
};

}
}

#endif
