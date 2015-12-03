/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_AST_INSERTER_H_
#define FLINT_DB_AST_INSERTER_H_

#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"
#include "db/statement-driver.hh"

namespace flint {
namespace db {

class AstInserter : StatementDriver {
public:
	AstInserter(const AstInserter &) = delete;
	AstInserter &operator=(const AstInserter &) = delete;

	explicit AstInserter(sqlite3 *db);

	~AstInserter();

	bool Insert(const boost::uuids::uuid &uuid, const char *name, const char *math);
};

}
}

#endif
