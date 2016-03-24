/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "phml/validator.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid_io.hpp>

using std::cerr;
using std::endl;

namespace flint {
namespace phml {

VariableDefinitionValidator::VariableDefinitionValidator(sqlite3 *db)
	: sd_diff_(db,
			   "SELECT m.module_id, m.name, p.type, p.pq_id, p.name FROM impls AS i"
			   " LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid"
			   " LEFT JOIN modules AS m ON p.module_rowid = m.rowid"
			   " WHERE p.type <> 'x' AND i.math LIKE '%(diff %'")
{
}

VariableDefinitionValidator::~VariableDefinitionValidator() = default;

namespace {

const char *GetTypeName(const unsigned char *type)
{
	switch (type[0]) {
	case 's': return "static-parameter";
	case 't': return "timeseries";
	case 'v': return "variable-parameter";
	default:
		assert(false);
		return nullptr;
	}
}

}

bool VariableDefinitionValidator::Validate()
{
	sqlite3_stmt *stmt = sd_diff_.stmt();
	int e = sqlite3_step(stmt);
	if (e == SQLITE_DONE)
		return true;
	if (e == SQLITE_ROW) {
		assert(sqlite3_column_bytes(stmt, 0) == boost::uuids::uuid::static_size());
		const void *module_id = sqlite3_column_blob(stmt, 0);
		boost::uuids::uuid uuid;
		std::memcpy(&uuid, module_id, uuid.size());
		const unsigned char *module_name = sqlite3_column_text(stmt, 1);
		assert(module_name);
		const unsigned char *pq_type = sqlite3_column_text(stmt, 2);
		assert(pq_type);
		int pq_id = sqlite3_column_int(stmt, 3);
		const unsigned char *pq_name = sqlite3_column_text(stmt, 4);
		assert(pq_name);
		cerr << "physical-quantity \""
			 << pq_name
			 << "\" ("
			 << pq_id
			 << ") of module \""
			 << module_name
			 << "\" ("
			 << uuid
			 << ") is of type "
			 << GetTypeName(pq_type)
			 << ", but defined with <diff>"
			 << endl;
	} else {
		cerr << "failed to step statement: " << e << endl;
	}
	return false;
}

}
}
