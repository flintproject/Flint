/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/variable.h"

#include <cassert>

namespace flint {

Variable::Variable(char type,
				   int id,
				   const char *name,
				   const char *unit,
				   int col,
				   int row,
				   double capacity,
				   bool independent)
	: type_(type)
	, id_(id)
	, name_(name)
	, unit_(unit)
	, col_(col)
	, row_(row)
	, capacity_(capacity)
	, independent_(independent)
{
	assert(unit);
}

}
