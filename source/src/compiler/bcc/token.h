/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_BCC_TOKEN_H_
#define FLINT_COMPILER_BCC_TOKEN_H_

#include <ostream>
#include <string>

namespace flint {
namespace compiler {
namespace bcc {

struct Token {
	enum class Type {
		kUnspecified,
		kAddress,
		kColon,
		kEol,
		kEqualSign,
		kFunction,
		kIdentifier,
		kIntReg,
		kLabel,
		kNumber,
		kParenClose,
		kParenOpen,
		kSpace,
		// functions
		kAbs,
		kAlloc,
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
		kBr,
		kCeiling,
		kCos,
		kCosh,
		kCot,
		kCoth,
		kCsc,
		kCsch,
		kDeref,
		kDeterminant,
		kDivide,
		kEq,
		kEulergamma,
		kExp,
		kExponentiale,
		kFactorial,
		kFalse,
		kFloor,
		kGeq,
		kGt,
		kJmp,
		kLb,
		kLc,
		kLd,
		kLeq,
		kLn,
		kLoad,
		kLoadi,
		kLog,
		kLog10,
		kLt,
		kMax,
		kMin,
		kMinus,
		kMmul,
		kMove,
		kMult,
		kNeq,
		kOuterproduct,
		kPi,
		kPlus,
		kPower,
		kRefer,
		kRem,
		kRet,
		kRoot,
		kSave,
		kScalarproduct,
		kSec,
		kSech,
		kSelect2,
		kSelect3,
		kSelrow,
		kSin,
		kSinh,
		kStore,
		kTan,
		kTanh,
		kTimes,
		kTranspose,
		kTrue,
		kVectorproduct,
		// procedure
		kMod,
		kExponentialVariate,
		kPoissonVariate,
		kGammaVariate,
		kGaussVariate,
		kLognormalVariate,
		kUniformVariate,
		kWeibullVariate
	};

	Type type;
	const char *lexeme;
	int size;

	std::ostream &Write(std::ostream &os) const;
};

}
}
}

#endif
