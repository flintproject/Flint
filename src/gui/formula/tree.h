/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_FORMULA_TREE_H_
#define FLINT_GUI_FORMULA_TREE_H_

#include <iostream>
#include <memory>
#include <vector>

#include "gui/formula/token.h"

namespace flint {
namespace gui {
namespace formula {

struct Tree {

	enum class Op {
		// terminal
		kCi,
		kCn,
		kCnNegative,
		// operator
		kAbs,
		kArccos,
		kArccosh,
		kArccot,
		kArccoth,
		kArccsc,
		kArccsch,
		kArcsec,
		kArcsech,
		kArcsin,
		kArcsinh,
		kArctan,
		kArctanh,
		kCeiling,
		kCos,
		kCosh,
		kCot,
		kCoth,
		kCsc,
		kCsch,
		kDivide,
		kExp,
		kFactorial,
		kFloor,
		kLn,
		kLog,
		kMax,
		kMean,
		kMin,
		kMinus,
		kPlus,
		kPower,
		kRem,
		kRoot,
		kSec,
		kSech,
		kSin,
		kSinh,
		kTan,
		kTanh,
		kTimes,
		// PRNG
		kExponentialVariate,
		kGammaVariate,
		kGaussVariate,
		kLognormalVariate,
		kPoissonVariate,
		kUniformVariate,
		kWeibullVariate
	};

	Op op;
	Token token;
	std::vector<std::unique_ptr<Tree> > children;

	void Write(const char *prefix, std::ostream &os) const;
};

}
}
}

#endif
