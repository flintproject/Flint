/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "eq-inserter.h"

#include <cstdio>
#include <cstring>
#include <iostream>

#include <boost/uuid/nil_generator.hpp>

namespace flint {
namespace db {

EqInserter::EqInserter(const char *table, sqlite3 *db)
	: query_(new char[128]) // long enough
	, stmt_(nullptr)
{
	std::sprintf(query_.get(),
			"INSERT INTO %s VALUES (?, ?)",
			table);
	int e;
	e = sqlite3_prepare_v2(db, query_.get(), -1, &stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e
			 << ": " << query_.get()
			 << std::endl;
	}
}

EqInserter::~EqInserter()
{
	sqlite3_finalize(stmt_);
}

bool EqInserter::Insert(const boost::uuids::uuid &uuid, const char *math)
{
	int e;
	e = sqlite3_bind_blob(stmt_, 1, &uuid, uuid.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind uuid: " << query_.get()
				  << ": " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(stmt_, 2, math, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind math: " << query_.get()
				  << ": " << e << std::endl;
		return false;
	}
	e = sqlite3_step(stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << query_.get()
				  << ": " << e << std::endl;
		return false;
	}
	sqlite3_reset(stmt_);
	return true;
}

bool EqInserter::Insert(const char *math)
{
	boost::uuids::uuid nu = boost::uuids::nil_uuid();
	return Insert(nu, math);
}

}
}
