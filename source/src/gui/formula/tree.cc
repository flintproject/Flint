/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/formula/tree.h"

namespace flint {
namespace gui {
namespace formula {

namespace {

const char *GetOpName(Tree::Op op)
{
	static const char *kMap[] = {
		// terminal
		"ci",
		"cn",
		"cn",
		// operator
		"abs",
		"arccos",
		"arccosh",
		"arccot",
		"arccoth",
		"arccsc",
		"arccsch",
		"arcsec",
		"arcsech",
		"arcsin",
		"arcsinh",
		"arctan",
		"arctanh",
		"ceiling",
		"cos",
		"cosh",
		"cot",
		"coth",
		"csc",
		"csch",
		"divide",
		"exp",
		"factorial",
		"floor",
		"ln",
		"log",
		"max",
		"mean",
		"min",
		"minus",
		"plus",
		"power",
		"rem",
		"root",
		"sec",
		"sech",
		"sin",
		"sinh",
		"tan",
		"tanh",
		"times",
		// PRNG
		"exponential_variate",
		"gamma_variate",
		"gauss_variate",
		"lognormal_variate",
		"poisson_variate",
		"uniform_variate",
		"weibull_variate"
	};
	return kMap[static_cast<int>(op)];
}

void StartTag(const char *prefix, const char *name, std::ostream &os)
{
	os << '<' << prefix << ':' << name << '>';
}

void EndTag(const char *prefix, const char *name, std::ostream &os)
{
	os << "</" << prefix << ':' << name << '>';
}

void StartApply(const char *prefix, const char *name, std::ostream &os)
{
	os << '<' << prefix << ":apply><" << prefix << ":" << name << "/>";
}

void StartApplyCsymbol(const char *prefix, const char *name, std::ostream &os)
{
	os << '<' << prefix << ":apply><" << prefix << ":csymbol>"
	   << name
	   << "</" << prefix << ":csymbol>";
}

void EndApply(const char *prefix, std::ostream &os)
{
	os << "</" << prefix << ":apply>";
}

}

void Tree::Write(const char *prefix, std::ostream &os) const
{
	switch (op) {
	case Op::kCi:
	case Op::kCn:
		StartTag(prefix, GetOpName(op), os);
		os.write(token.lexeme, token.size);
		EndTag(prefix, GetOpName(op), os);
		break;
	case Op::kCnNegative:
		StartTag(prefix, GetOpName(op), os);
		os.put('-');
		os.write(token.lexeme, token.size);
		EndTag(prefix, GetOpName(op), os);
		break;
	case Op::kAbs:
	case Op::kArccos:
	case Op::kArccosh:
	case Op::kArccot:
	case Op::kArccoth:
	case Op::kArccsc:
	case Op::kArccsch:
	case Op::kArcsec:
	case Op::kArcsech:
	case Op::kArcsin:
	case Op::kArcsinh:
	case Op::kArctan:
	case Op::kArctanh:
	case Op::kCeiling:
	case Op::kCos:
	case Op::kCosh:
	case Op::kCot:
	case Op::kCoth:
	case Op::kCsc:
	case Op::kCsch:
	case Op::kDivide:
	case Op::kExp:
	case Op::kFactorial:
	case Op::kFloor:
	case Op::kLn:
	case Op::kLog:
	case Op::kMax:
	case Op::kMean:
	case Op::kMin:
	case Op::kMinus:
	case Op::kPlus:
	case Op::kPower:
	case Op::kRem:
	case Op::kRoot:
	case Op::kSec:
	case Op::kSech:
	case Op::kSin:
	case Op::kSinh:
	case Op::kTan:
	case Op::kTanh:
	case Op::kTimes:
		StartApply(prefix, GetOpName(op), os);
		for (const auto &child : children)
			child->Write(prefix, os);
		EndApply(prefix, os);
		break;
	case Op::kExponentialVariate:
	case Op::kGammaVariate:
	case Op::kGaussVariate:
	case Op::kLognormalVariate:
	case Op::kPoissonVariate:
	case Op::kUniformVariate:
	case Op::kWeibullVariate:
		StartApplyCsymbol(prefix, GetOpName(op), os);
		for (const auto &child : children)
			child->Write(prefix, os);
		EndApply(prefix, os);
		break;
	}
}

}
}
}
