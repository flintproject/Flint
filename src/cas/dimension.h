/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_DIMENSION_H_
#define FLINT_CAS_DIMENSION_H_

#include <memory>

#include <boost/uuid/uuid.hpp>

#include "cas.h"
#include "sqlite3.h"

namespace flint {

class VariableMap;

namespace cas {

class DimensionAnalyzer {
public:
	DimensionAnalyzer();
	~DimensionAnalyzer();

	/*
	 * Return true in case of success, false otherwise.
	 */
	bool Analyse(const boost::uuids::uuid &uuid, Expr *expr,
				 int *col, int *row) const;

	bool Load(sqlite3 *db);

private:
	std::unique_ptr<VariableMap> vm_;
};

}
}

#endif
