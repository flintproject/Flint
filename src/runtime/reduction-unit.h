/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_REDUCTION_UNIT_H_
#define FLINT_RUNTIME_REDUCTION_UNIT_H_

#include <unordered_set>

#include "reduction.h"

namespace flint {

class ReductionUnit {
public:
	ReductionUnit(Reduction reduction, int target_addr, int size);

	Reduction reduction() const {return reduction_;}
	int target_addr() const {return target_addr_;}
	const std::unordered_set<int> &source_addrs() const {return source_addrs_;}
	int size() const {return size_;}

	void AddSourceAddr(int source_addr);

	bool operator()(double *data) const;

private:
	Reduction reduction_;
	int target_addr_;
	std::unordered_set<int> source_addrs_;
	int size_;
};

}

#endif
