/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_REDUCTION_HH_
#define FLINT_REDUCTION_HH_

namespace flint {

enum class Reduction {
	kUnspecified,
	kSum,
	kMax,
	kMin,
	kMean,
	kDegree
};

/*
 * Return true in case of success, false otherwise.
 */
bool ConvertStringToReduction(const char *s, Reduction *r);

}

#endif
