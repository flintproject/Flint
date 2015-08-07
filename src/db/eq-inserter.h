/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_EQ_INSERTER_H_
#define FLINT_DB_EQ_INSERTER_H_

#include <memory>

#include <boost/noncopyable.hpp>
#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace db {

class EqInserter : boost::noncopyable {
public:
	EqInserter(const char *table, sqlite3 *db);

	~EqInserter();

	bool Insert(const boost::uuids::uuid &uuid, const char *math);

	/*
	 * Call Insert() with the default nil UUID.
	 */
	bool Insert(const char *math);

private:
	std::unique_ptr<char[]> query_;
	sqlite3_stmt *stmt_;
};

} // namespace db

#endif
