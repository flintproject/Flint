/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_NAME_INSERTER_H_
#define FLINT_DB_NAME_INSERTER_H_

#include <memory>

#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace flint {
namespace db {

class NameInserter {
public:
	NameInserter(const NameInserter &) = delete;
	NameInserter &operator=(const NameInserter &) = delete;

	NameInserter(const char *table, sqlite3 *db);

	~NameInserter();

	bool InsertName(const boost::uuids::uuid &space_id, char type, int id, const char *name);

	/*
	 * Call InsertName() with the default nil UUID.
	 */
	bool InsertName(char type, int id, const char *name);

private:
	std::unique_ptr<char[]> query_;
	sqlite3_stmt *stmt_;
};

}
}

#endif
