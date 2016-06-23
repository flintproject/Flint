/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "calculation-dependency.h"

#include <cstdio>
#include <iostream>

#include "calculation-unit.h"

using std::cerr;
using std::endl;

namespace flint {

CalculationDependency::CalculationDependency(int section_index, int block_index,
											 int offset, int cib, int cie)
	: cu_(new CalculationUnit(section_index, block_index,
							  offset, cib, cie))
	, load_addrs_()
	, store_addr_(-1)
{
}

void CalculationDependency::AddLoadAddress(int load_addr)
{
	load_addrs_.insert(load_addr);
}

void CalculationDependency::SetStoreAddr(int store_addr)
{
	if (store_addr_ >= 0) {
		cerr << "more than one store addr found: " << store_addr_ << endl;
	}
	store_addr_ = store_addr;
}

}