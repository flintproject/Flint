/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_TAC_INSERTER_HH_
#define FLINT_DB_TAC_INSERTER_HH_

#include <boost/uuid/uuid.hpp>

#include "statement-driver.hh"

namespace flint {
namespace db {

class TacInserter : StatementDriver {
public:
	explicit TacInserter(sqlite3 *db);

	bool Insert(const boost::uuids::uuid &uuid, const char *name, int nod, const char *body);

	/*
	 * Call Insert() with the default nil UUID.
	 */
	bool Insert(const char *name, int nod, const char *body);
};

}
}

#endif
