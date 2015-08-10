/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "reduction-unit.hh"

#include <algorithm>
#include <cstdio>
#include <iostream>
#include <numeric>

using std::cerr;
using std::endl;

namespace flint {

ReductionUnit::ReductionUnit(Reduction reduction, int target_addr)
	: reduction_(reduction)
	, target_addr_(target_addr)
	, source_addrs_()
{}

void ReductionUnit::AddSourceAddr(int source_addr)
{
	source_addrs_.insert(source_addr);
}

bool ReductionUnit::operator()(double *data) const
{
	double d;
	switch (reduction_) {
	case Reduction::kUnspecified:
		cerr << "reduction is unspecified" << endl;
		return false;
	case Reduction::kSum:
		d = 0;
		for (auto a : source_addrs_)
			d += data[a];
		break;
	case Reduction::kMax:
		{
			auto it = source_addrs_.cbegin();
			if (it == source_addrs_.cend()) {
				cerr << "failed to take max due to no input" << endl;
				return false;
			}
			d = data[*it];
			while (++it != source_addrs_.cend())
				d = std::max(d, data[*it]);
		}
		break;
	case Reduction::kMin:
		{
			auto it = source_addrs_.cbegin();
			if (it == source_addrs_.cend()) {
				cerr << "failed to take min due to no input" << endl;
				return false;
			}
			d = data[*it];
			while (++it != source_addrs_.cend()) {
				d = std::min(d, data[*it]);
			}
		}
		break;
	case Reduction::kMean:
		{
			auto it = source_addrs_.cbegin();
			if (it == source_addrs_.cend()) {
				cerr << "failed to take mean due to no input" << endl;
				return false;
			}
			d = data[*it];
			while (++it != source_addrs_.cend())
				d += data[*it];
			d /= source_addrs_.size();
		}
		break;
	case Reduction::kDegree:
		d = source_addrs_.size();
		break;
	}
	data[target_addr_] = d;
	return true;
}

}