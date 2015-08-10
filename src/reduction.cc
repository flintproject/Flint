/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "reduction.hh"

#include <cstdio>
#include <cstring>
#include <iostream>

using std::cerr;
using std::endl;
using std::strcmp;

namespace flint {

bool ConvertStringToReduction(const char *s, Reduction *r)
{
	if (!s)	{
		*r = Reduction::kUnspecified;
		return true;
	}
	if (strcmp(s, "sum") == 0) {
		*r = Reduction::kSum;
	} else if (strcmp(s, "max") == 0) {
		*r = Reduction::kMax;
	} else if (strcmp(s, "min") == 0) {
		*r = Reduction::kMin;
	} else if (strcmp(s, "mean") == 0) {
		*r = Reduction::kMean;
	} else if (strcmp(s, "degree") == 0) {
		*r = Reduction::kDegree;
	} else {
		cerr << "unknown reduction: " << s << endl;
		return false;
	}
	return true;
}

}
