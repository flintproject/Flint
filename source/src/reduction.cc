/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "reduction.h"

#include <cstdio>
#include <cstring>
#include <iostream>

namespace flint {

bool ConvertStringToReduction(const char *s, Reduction *r)
{
	if (!s)	{
		*r = Reduction::kUnspecified;
		return true;
	}
	if (std::strcmp(s, "sum") == 0) {
		*r = Reduction::kSum;
	} else if (std::strcmp(s, "max") == 0) {
		*r = Reduction::kMax;
	} else if (std::strcmp(s, "min") == 0) {
		*r = Reduction::kMin;
	} else if (std::strcmp(s, "mean") == 0) {
		*r = Reduction::kMean;
	} else if (std::strcmp(s, "degree") == 0) {
		*r = Reduction::kDegree;
	} else {
		std::cerr << "unknown reduction: " << s << std::endl;
		return false;
	}
	return true;
}

}
