/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "runtime/calculation-unit.h"

#include <cassert>

namespace flint {

CalculationUnit::CalculationUnit(int section_index, int block_index,
								 int offset, int cib, int cie)
	: section_index_(section_index)
	, block_index_(block_index)
	, offset_(offset)
	, cib_(cib)
	, cie_(cie)
{
	assert(cib < cie);
}

}
