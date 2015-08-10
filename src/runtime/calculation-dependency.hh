/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_CALCULATION_DEPENDENCY_HH_
#define FLINT_RUNTIME_CALCULATION_DEPENDENCY_HH_

#include <memory>
#include <unordered_set>

namespace flint {

class CalculationUnit;

class CalculationDependency {
public:
	CalculationDependency(int section_index, int sector_index, int block_index);

	const CalculationUnit &cu() const {return *cu_;}
	const std::unordered_set<int> &load_addrs() const {return load_addrs_;}
	int store_addr() const {return store_addr_;}

	void AddLoadAddress(int load_addr);

	void SetStoreAddr(int store_addr);

private:
	std::unique_ptr<CalculationUnit> cu_;
	std::unordered_set<int> load_addrs_;
	int store_addr_;
};

}

#endif
