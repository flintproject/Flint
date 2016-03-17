/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_EXECUTION_UNIT_H_
#define FLINT_RUNTIME_EXECUTION_UNIT_H_

#include <boost/variant.hpp>

#include "calculation-unit.h"
#include "reduction-unit.h"

namespace flint {

typedef boost::variant<CalculationUnit, ReductionUnit> ExecutionUnit;

}

#endif
