/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/tac/context.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <boost/uuid/uuid_io.hpp>

#include "lexer.h"

using std::cerr;
using std::endl;

namespace flint {
namespace compiler {
namespace tac {

namespace {

bool IsAnd(const cas::Compound &c)
{
	return c.keyword == "and";
}

bool IsOr(const cas::Compound &c)
{
	return c.keyword == "or";
}

bool IsAt(const cas::Compound &c)
{
	return c.keyword == "$At";
}

bool IsLookback(const cas::Compound &c)
{
	return c.keyword == "$lookback";
}

bool IsPiecewise(const cas::Compound &c)
{
	return c.keyword == "piecewise";
}

bool IsPiece(const cas::Compound &c)
{
	return c.keyword == "piece";
}

bool IsOtherwise(const cas::Compound &c)
{
	return c.keyword == "otherwise";
}

bool IsTrial(const cas::Compound &c)
{
	return c.keyword == "$trial";
}

bool IsOutcome(const cas::Compound &c)
{
	return c.keyword == "$outcome";
}

struct KeyFun {
	std::string keyword;
	bool (Context::*function)(RegisterType rt, int n, cas::Compound &c);
};


const KeyFun kKeyFun[] = {
	// Keep the following entries in bibliographical order.
	{"$Mod", &Context::NaryScalarToScalar},
	{"$exponential_variate", &Context::NaryScalarToScalar},
	{"$gamma_variate", &Context::NaryScalarToScalar},
	{"$gauss_variate", &Context::NaryScalarToScalar},
	{"$lognormal_variate", &Context::NaryScalarToScalar},
	{"$poisson_variate", &Context::NaryScalarToScalar},
	{"$uniform_variate", &Context::NaryScalarToScalar},
	{"$weibull_variate", &Context::NaryScalarToScalar},
	{"abs", &Context::NaryScalarToScalar},
	{"and", &Context::NaryScalarToScalar},
	{"arccos", &Context::NaryScalarToScalar},
	{"arccosh", &Context::NaryScalarToScalar},
	{"arccot", &Context::NaryScalarToScalar},
	{"arccoth", &Context::NaryScalarToScalar},
	{"arccsc", &Context::NaryScalarToScalar},
	{"arccsch", &Context::NaryScalarToScalar},
	{"arcsec", &Context::NaryScalarToScalar},
	{"arcsech", &Context::NaryScalarToScalar},
	{"arcsin", &Context::NaryScalarToScalar},
	{"arcsinh", &Context::NaryScalarToScalar},
	{"arctan", &Context::NaryScalarToScalar},
	{"arctanh", &Context::NaryScalarToScalar},
	{"ceiling", &Context::NaryScalarToScalar},
	{"cos", &Context::NaryScalarToScalar},
	{"cosh", &Context::NaryScalarToScalar},
	{"cot", &Context::NaryScalarToScalar},
	{"coth", &Context::NaryScalarToScalar},
	{"csc", &Context::NaryScalarToScalar},
	{"csch", &Context::NaryScalarToScalar},
	{"determinant", &Context::Determinant},
	{"divide", &Context::NaryScalarToScalar},
	{"eq", &Context::Componentwise},
	{"exp", &Context::NaryScalarToScalar},
	{"floor", &Context::NaryScalarToScalar},
	{"geq", &Context::NaryScalarToScalar},
	{"gt", &Context::NaryScalarToScalar},
	{"leq", &Context::NaryScalarToScalar},
	{"ln", &Context::NaryScalarToScalar},
	{"log", &Context::NaryScalarToScalar},
	{"log10", &Context::NaryScalarToScalar},
	{"lt", &Context::NaryScalarToScalar},
	{"matrix", &Context::Matrix},
	{"max", &Context::NaryScalarToScalar},
	{"mean", &Context::NaryScalarToScalar},
	{"min", &Context::NaryScalarToScalar},
	{"minus", &Context::Componentwise},
	{"neq", &Context::Componentwise},
	{"not", &Context::NaryScalarToScalar},
	{"or", &Context::NaryScalarToScalar},
	{"outerproduct", &Context::Outerproduct},
	{"plus", &Context::Componentwise},
	{"power", &Context::NaryScalarToScalar}, // TODO: matrix
	{"rem", &Context::NaryScalarToScalar},
	{"root", &Context::NaryScalarToScalar},
	{"scalarproduct", &Context::Scalarproduct},
	{"sec", &Context::NaryScalarToScalar},
	{"sech", &Context::NaryScalarToScalar},
	{"selector", &Context::Selector},
	{"sin", &Context::NaryScalarToScalar},
	{"sinh", &Context::NaryScalarToScalar},
	{"tan", &Context::NaryScalarToScalar},
	{"tanh", &Context::NaryScalarToScalar},
	{"times", &Context::Times},
	{"transpose", &Context::Transpose},
	{"vector", &Context::Vector},
	{"vectorproduct", &Context::Vectorproduct},
	{"xor", &Context::NaryScalarToScalar}
};

}

Context::Context(const boost::uuids::uuid &uuid,
				 const char *id,
				 std::ostream *os)
	: uuid_(uuid)
	, id_(id)
	, ir_(0)
	, fr_(0)
	, l_(0)
	, os_(os)
{}

Context::~Context() = default;

bool Context::EmitCode(cas::Expr &expr)
{
	int w, col, row;
	GetType(expr, &w, &col, &row);
	if (col == 1 && row == 1) {
		fr_++;
		if (!Assign(RegisterType::kFloat, 0, expr))
			return false;
		*os_ << "  store " << id_ << " $0" << endl;
	} else {
		ir_++;
		if (!Assign(RegisterType::kInteger, 0, expr))
			return false;
		*os_ << "  save " << id_ << " $i0 " << (col * row) << endl;
	}
	return true;
}

bool Context::Componentwise(RegisterType rt, int n, cas::Compound &c)
{
	if (rt == RegisterType::kFloat)
		return NaryScalarToScalar(rt, n, c);

	int size = c.col * c.row;
	std::vector<int> args;
	for (auto &e : c.children) {
		int p = ir_++;
		if (!Assign(RegisterType::kInteger, p, e))
			return false;
		args.push_back(p);
	}
	*os_ << "  alloc $i" << n << ' ' << size << endl;
	for (int i=0;i<size;i++) {
		std::vector<int> params;
		for (auto k : args) {
			int m = fr_++;
			*os_ << "  deref $" << m << " $i" << k << ' ' << i << endl;
			params.push_back(m);
		}
		int m = fr_++;
		*os_ << "  $" << m << " = (" << c.keyword;
		for (auto k : params)
			*os_ << " $" << k;
		*os_ << ')' << endl;
		*os_ << "  move $i" << n << " $" << m << ' ' << i << endl;
	}
	return true;
}

bool Context::NaryScalarToScalar(RegisterType /*rt*/, int n, cas::Compound &c)
{
	std::vector<int> args;
	for (auto &e : c.children) {
		int m = fr_++;
		if (!Assign(RegisterType::kFloat, m, e))
			return false;
		args.push_back(m);
	}
	*os_ << "  $" << n << " = (" << c.keyword;
	for (auto i : args)
		*os_ << " $" << i;
	*os_ << ')' << endl;
	return true;
}

bool Context::Determinant(RegisterType rt, int n, cas::Compound &c)
{
	assert(rt == RegisterType::kFloat);
	assert(c.children.size() == 1);
	int col, row;
	GetType(c.children[0], nullptr, &col, &row);
	assert(col == row);
	int n1 = ir_++;
	if (!Assign(RegisterType::kInteger, n1, c.children[0]))
		return false;
	*os_ << "  determinant $" << n << ' ' << col << " $i" << n1 << endl;
	return true;
}

bool Context::Matrix(RegisterType rt, int n, cas::Compound &c)
{
	assert(rt == RegisterType::kInteger);
	*os_ << "  alloc $i" << n << ' ' << (c.col * c.row) << endl;
	int i=0;
	for (auto &e : c.children) {
		assert(e.which() == cas::kExprIsCompound);
		cas::Compound &rc(boost::get<cas::Compound>(e));
		assert(rc.keyword == "matrixrow");
		for (auto &rcc : rc.children) {
			int m = fr_++;
			if (!Assign(RegisterType::kFloat, m, rcc))
				return false;
			*os_ << "  move $i" << n << " $" << m << ' ' << i++ << endl;
		}
	}
	return true;
}

bool Context::Outerproduct(RegisterType rt, int n, cas::Compound &c)
{
	assert(rt == RegisterType::kInteger);
	int n1 = ir_++;
	if (!Assign(RegisterType::kInteger, n1, c.children[0]))
		return false;
	int n2 = ir_++;
	if (!Assign(RegisterType::kInteger, n2, c.children[1]))
		return false;
	*os_ << "  outerproduct $i" << n
		 << ' ' << c.row << " $i" << n1
		 << ' ' << c.col << " $i" << n2
		 << endl;
	return true;
}

bool Context::Scalarproduct(RegisterType rt, int n, cas::Compound &c)
{
	assert(rt == RegisterType::kFloat);
	int col, row;
	GetType(c.children[0], nullptr, &col, &row);
	int n1 = ir_++;
	if (!Assign(RegisterType::kInteger, n1, c.children[0]))
		return false;
	int n2 = ir_++;
	if (!Assign(RegisterType::kInteger, n2, c.children[1]))
		return false;
	int size = std::max(col, row);
	*os_ << "  scalarproduct $" << n << ' ' << size << " $i" << n1 << " $i" << n2 << endl;
	return true;
}

bool Context::Selector(RegisterType rt, int n, cas::Compound &c)
{
	size_t size = c.children.size();
	assert(size >= 2);
	int n0 = ir_++;
	if (!Assign(RegisterType::kInteger, n0, c.children[0]))
		return false;
	int m1 = fr_++;
	if (!Assign(RegisterType::kFloat, m1, c.children[1]))
		return false;
	if (rt == RegisterType::kInteger) {
		assert(size == 2);
		int col0, row0;
		GetType(c.children[0], nullptr, &col0, &row0);
		*os_ << "  selrow $i" << n
			 << ' ' << row0
			 << ' ' << col0
			 << " $i" << n0
			 << " $" << m1 << endl;
	} else if (size == 2) {
		*os_ << "  select2 $" << n << " $i" << n0 << " $" << m1 << endl;
	} else {
		assert(size == 3);
		int col0, row0;
		GetType(c.children[0], nullptr, &col0, &row0);
		int m2 = fr_++;
		if (!Assign(RegisterType::kFloat, m2, c.children[2]))
			return false;
		*os_ << "  select3 $" << n
			 << ' ' << row0
			 << ' ' << col0
			 << " $i" << n0
			 << " $" << m1
			 << " $" << m2
			 << endl;
	}
	return true;
}

bool Context::Times(RegisterType rt, int n, cas::Compound &c)
{
	if (rt == RegisterType::kFloat)
		return NaryScalarToScalar(rt, n, c);
	assert(c.children.size() == 2);
	cas::Expr &e0(c.children.at(0));
	cas::Expr &e1(c.children.at(1));
	int w0, w1;
	int col0, col1;
	int row0, row1;
	GetType(e0, &w0, &col0, &row0);
	GetType(e1, &w1, &col1, &row1);
	if (col0 == 1 && row0 == 1) { // when the 1st operand is scalar
		int m0 = fr_++;
		if (!Assign(RegisterType::kFloat, m0, e0))
			return false;
		int n1 = ir_++;
		if (!Assign(RegisterType::kInteger, n1, e1))
			return false;
		int size = col1 * row1;
		*os_ << "  mult $i" << n << ' ' << size << " $" << m0 << " $i" << n1 << endl;
	} else if (col1 == 1 && row1 == 1) { // when the 2nd operand is scalar
		int n0 = ir_++;
		if (!Assign(RegisterType::kInteger, n0, e0))
			return false;
		int m1 = fr_++;
		if (!Assign(RegisterType::kFloat, m1, e1))
			return false;
		int size = col0 * row0;
		*os_ << "  mult $i" << n << ' ' << size << " $" << m1 << " $i" << n0 << endl;
	} else { // both are matrices
		int n0 = ir_++;
		if (!Assign(RegisterType::kInteger, n0, e0))
			return false;
		int n1 = ir_++;
		if (!Assign(RegisterType::kInteger, n1, e1))
			return false;
		*os_ << "  mmul $i" << n
			 << ' ' << row0
			 << ' ' << col0 // = row1
			 << ' ' << col1
			 << " $i" << n0
			 << " $i" << n1
			 << endl;
	}
	return true;
}

bool Context::Transpose(RegisterType rt, int n, cas::Compound &c)
{
	assert(rt == RegisterType::kInteger);
	int n1 = ir_++;
	if (!Assign(RegisterType::kInteger, n1, c.children[0]))
		return false;
	*os_ << "  transpose $i" << n
		 << " $i" << n1
		 << ' ' << c.row
		 << ' ' << c.col
		 << endl;
	return true;
}

bool Context::Vector(RegisterType rt, int n, cas::Compound &c)
{
	assert(rt == RegisterType::kInteger);
	int size = c.col * c.row;
	*os_ << "  alloc $i" << n << ' ' << size << endl;
	for (int i=0;i<size;i++) {
		int m = fr_++;
		if (!Assign(RegisterType::kFloat, m, c.children[i]))
			return false;
		*os_ << "  move $i" << n << " $" << m << ' ' << i << endl;
	}
	return true;
}

bool Context::Vectorproduct(RegisterType rt, int n, cas::Compound &c)
{
	assert(rt == RegisterType::kInteger);
	int n1 = ir_++;
	if (!Assign(RegisterType::kInteger, n1, c.children[0]))
		return false;
	int n2 = ir_++;
	if (!Assign(RegisterType::kInteger, n2, c.children[1]))
		return false;
	*os_ << "  vectorproduct $i" << n << " $i" << n1 << " $i" << n2 << endl;
	return true;
}

/*
 * The control should reach the end when the expr is evaluated to be false.
 *
 * @param n the variable for the resulting boolean value
 * @param l the label to go when the expr is evaluated to be true
 */
bool Context::EmitCondition(int n, int l, cas::Expr &expr)
{
	if (expr.which() == cas::kExprIsCompound) {
		cas::Compound &comp(boost::get<cas::Compound>(expr));
		if (IsAnd(comp)) {
			int l1 = l_++;
			int l2 = l_++;
			if (!EmitCondition(n, l2, comp.children.at(0)))
				return false;
			*os_ << "  jmp L" << l1 << endl;
			*os_ << " L" << l2 << ':' << endl;
			if (!EmitCondition(n, l, comp.children.at(1)))
				return false;
			*os_ << " L" << l1 << ':' << endl;
			return bool(*os_);
		} else if (IsOr(comp)) {
			return EmitCondition(n, l, comp.children.at(0))
				&& EmitCondition(n, l, comp.children.at(1));
		}
	}

	if (!Assign(RegisterType::kFloat, n, expr))
		return false;
	*os_ << "  br $" << n << " L" << l << endl;
	return bool(*os_);
}

bool Context::EmitAt(int n, std::deque<cas::Expr> &children)
{
	size_t len = children.size();
	if (len > 3) {
		cerr << "error: more than 3 arguments: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	if (len < 3) {
		cerr << "error: EmitAt: missing arguments: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	if (children.at(0).which() != cas::kExprIsInteger) {
		cerr << "error: invalid 1st argument of At: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	if (children.at(1).which() != cas::kExprIsInteger) {
		cerr << "error: invalid 2nd argument of At: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	int m = fr_++;
	if (!Assign(RegisterType::kFloat, m, children.at(2))) // TODO: RegisterType::kInteger
		return false;
	*os_ << "  ld $"
		 << n
		 << ' '
		 << boost::get<int>(children.at(0))
		 << ' '
		 << boost::get<int>(children.at(1))
		 << " $"
		 << m
		 << endl;
	return bool(*os_);
}

bool Context::EmitLookback(int n, std::deque<cas::Expr> &children)
{
	size_t len = children.size();
	if (len > 2) {
		cerr << "error: more than 2 arguments: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	if (len < 2) {
		cerr << "error: EmitLookback: missing arguments: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	if (children.at(0).which() != cas::kExprIsIdentifier) {
		cerr << "error: invalid 1st argument of Delay/DeltaTime: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	int m = fr_++;
	if (!Assign(RegisterType::kFloat, m, children.at(1))) // TODO: vector/matrix
		return false;
	*os_ << "  lb $"
		 << n
		 << ' '
		 << boost::get<cas::Identifier>(children.at(0)).name.c_str()
		 << " $"
		 << m
		 << endl;
	return bool(*os_);
}

bool Context::Piecewise(RegisterType rt, int n, std::deque<cas::Expr> &children)
{
	int l = l_++;
	std::vector<int> v1;
	bool otherwise = false;
	for (auto &expr : children) {
		assert(expr.which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(expr));
		if (IsPiece(comp)) {
			int l1 = l_++;
			int m = fr_++;
			if (!EmitCondition(m, l1, comp.children.at(1)))
				return false;
			v1.push_back(l1);
		} else if (IsOtherwise(comp)) {
			otherwise = true;
			if (!Assign(rt, n, comp.children.at(0)))
				return false;
			*os_ << "  jmp L" << l << endl;
		} else {
			assert(false);
		}
	}
	if (!otherwise)
		*os_ << "  ret" << endl;
	auto v1it = v1.cbegin();
	for (auto &expr : children) {
		assert(expr.which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(expr));
		if (IsPiece(comp)) {
			*os_ << " L" << *v1it++ << ':' << endl;
			if (!Assign(rt, n, comp.children.at(0)))
				return false;
			*os_ << "  jmp L" << l << endl;
		} else if (IsOtherwise(comp)) {
			/* nothing to do */
		} else {
			assert(false);
		}
	}
	*os_ << " L" << l << ':' << endl;
	return bool(*os_);
}

bool Context::EmitTrial(int n, std::deque<cas::Expr> &children)
{
	int l = l_++;
	int p0 = fr_++;
	int p1 = fr_++;
	int p = fr_++;
	*os_ << "  loadi $" << p0 << " 0" << endl
		 << "  loadi $" << p1 << " 1" << endl
		 << "  $" << p << " = ($uniform_variate $" << p0 << " $" << p1 << ')' << endl;
	std::vector<int> v1;
	for (auto &expr : children) {
		assert(expr.which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(expr));
		if (IsOutcome(comp)) {
			int l1 = l_++;
			int m0 = fr_++;
			if (!Assign(RegisterType::kFloat, m0, comp.children.at(1)))
				return false;
			int m1 = fr_++;
			*os_ << "  $" << m1 << " = (minus $" << p << " $" << m0 << ')' << endl;
			p = m1; // keep the register number for the decreased value as p
			int m2 = fr_++;
			*os_ << "  $" << m2 << " = (leq $" << m1 << " $" << p0 << ')' << endl
				 << "  br $" << m2 << " L" << l1 << endl;
			v1.push_back(l1);
		} else {
			cerr << "error: unexpected child of $trial: " << uuid_ << ' ' << id_ << endl;
			return false;
		}
	}
	*os_ << "  ret" << endl;
	auto v1it = v1.cbegin();
	for (auto &expr : children) {
		assert(expr.which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(expr));
		if (IsOutcome(comp)) {
			*os_ << " L" << *v1it++ << ':' << endl;
			if (!Assign(RegisterType::kFloat, n, comp.children.at(0)))
				return false;
			*os_ << "  jmp L" << l << endl;
		} else {
			assert(false);
		}
	}
	*os_ << " L" << l << ':' << endl;
	return bool(*os_);
}

bool Context::Assign(RegisterType rt, int n, cas::Expr &expr)
{
	switch (expr.which()) {
	case cas::kExprIsCompound:
		return Assign(rt, n, boost::get<cas::Compound>(expr));
	case cas::kExprIsIdentifier:
		return Assign(rt, n, boost::get<cas::Identifier>(expr));
	case cas::kExprIsInteger:
		assert(rt == RegisterType::kFloat);
		return Assign(n, boost::get<int>(expr));
	case cas::kExprIsReal:
		assert(rt == RegisterType::kFloat);
		return Assign(n, boost::get<lexer::Real>(expr));
	default:
		assert(false);
		return false;
	}
}

bool Context::Assign(RegisterType rt, int n, cas::Compound &c)
{
	if (IsAt(c))
		return EmitAt(n, c.children);
	if (IsLookback(c))
		return EmitLookback(n, c.children);
	if (IsPiecewise(c))
		return Piecewise(rt, n, c.children);
	if (IsTrial(c))
		return EmitTrial(n, c.children);

	size_t len = c.children.size();
	if (len < 1) {
		cerr << "error: missing arguments for " << c.keyword << ": "
			 << uuid_ << ' ' << id_ << endl;
		return false;
	}
	for (const auto &kf : kKeyFun) { // TODO: faster search e.g. via bsearch?
		if (c.keyword == kf.keyword)
			return (this->*(kf.function))(rt, n, c);
	}
	cerr << "unsupported function: " << c.keyword << endl;
	return false;
}

bool Context::Assign(RegisterType rt, int n, const cas::Identifier &id)
{
	const std::string &s(id.name);
	if (rt == RegisterType::kInteger) {
		*os_ << "  refer $i" << n << " " << id.name << endl;
	} else {
		if ( (s[0] == '%' || s[0] == '@') && isalpha(s[1]) ) {
			*os_ << "  load $" << n << ' ' << s << endl;
		} else {
			*os_ << "  loadi $" << n << ' ' << s << endl;
		}
	}
	return true;
}

bool Context::Assign(int n, int i)
{
	*os_ << "  loadi $" << n << ' ' << i << endl;
	return true;
}

bool Context::Assign(int n, const lexer::Real &r)
{
	*os_ << "  loadi $" << n << ' ' << r.lexeme << endl;
	return true;
}

void Context::GetType(cas::Expr &expr, int *w, int *col, int *row)
{
	int which = expr.which();
	switch (which) {
	case cas::kExprIsCompound:
		{
			cas::Compound &c(boost::get<cas::Compound>(expr));
			if (col)
				*col = c.col;
			if (row)
				*row = c.row;
		}
		break;
	case cas::kExprIsIdentifier:
		{
			const cas::Identifier &id(boost::get<const cas::Identifier>(expr));
			if (col)
				*col = id.col;
			if (row)
				*row = id.row;
		}
		break;
	default:
		if (col)
			*col = 1;
		if (row)
			*row = 1;
		break;
	}
	if (w)
		*w = which;
}

}
}
}
