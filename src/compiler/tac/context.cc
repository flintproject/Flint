/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/tac/context.h"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <boost/uuid/uuid_io.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "lexer.hh"

using std::cerr;
using std::endl;

namespace flint {
namespace compiler {
namespace tac {

namespace {

class Printer : public boost::static_visitor<> {
public:
	explicit Printer(std::ostream *os)
		: os_(os)
	{}

	void operator()(const cas::Compound &c) const {
		*os_ << '(' << c.keyword.c_str();
		auto bit = c.children.cbegin();
		auto eit = c.children.cend();
		for (auto it=bit;it!=eit;++it) {
			os_->put(' ');
			boost::apply_visitor(*this, *it);
		}
		os_->put(')');
	}

	void operator()(const std::string &s) const {
		*os_ << s;
	}

	void operator()(int i) const {
		*os_ << i;
	}

	void operator()(const flint::lexer::Real &r) const {
		*os_ << r.lexeme;
	}

private:
	std::ostream *os_;
};

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

}

Context::Context(const boost::uuids::uuid &uuid,
				 const char *id,
				 std::ostream *os)
	: uuid_(uuid)
	, id_(id)
	, avail_n_(1)
	, avail_l_(0)
	, os_(os)
{}

Context::~Context() = default;

bool Context::EmitCode(cas::Expr &expr)
{
	if (!EmitCode(0, expr))
		return false;
	*os_ << "  store " << id_ << " $0" << endl;
	return true;
}

/*
 * The control should reach the end when the sexp is evaluated to be false.
 *
 * @param n the variable for the resulting boolean value
 * @param l the label to go when the sexp is evaluated to be true
 */
bool Context::EmitCondition(int n, int l, cas::Expr &sexp)
{
	if (sexp.which() == cas::kExprIsCompound) {
		cas::Compound &comp(boost::get<cas::Compound>(sexp));
		if (IsAnd(comp)) {
			int l1 = avail_l_++;
			int l2 = avail_l_++;
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

	if (!EmitCode(n, sexp))
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
	int m = avail_n_++;
	if (!EmitCode(m, children.at(2)))
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
	if (children.at(0).which() != cas::kExprIsString) {
		cerr << "error: invalid 1st argument of Delay/DeltaTime: " << uuid_ << ' ' << id_ << endl;
		return false;
	}
	int m = avail_n_++;
	if (!EmitCode(m, children.at(1)))
		return false;
	*os_ << "  lb $"
		 << n
		 << ' '
		 << boost::get<std::string>(children.at(0)).c_str()
		 << " $"
		 << m
		 << endl;
	return bool(*os_);
}

bool Context::EmitPiecewise(int n, std::deque<cas::Expr> &children)
{
	int l = avail_l_++;
	std::vector<int> v1;
	bool otherwise = false;
	for (std::deque<cas::Expr>::iterator it=children.begin();it!=children.end();++it) {
		assert(it->which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(*it));
		if (IsPiece(comp)) {
			int l1 = avail_l_++;
			int m = avail_n_++;
			if (!EmitCondition(m, l1, comp.children.at(1)))
				return false;
			v1.push_back(l1);
		} else if (IsOtherwise(comp)) {
			otherwise = true;
			if (!EmitCode(n, comp.children.at(0)))
				return false;
			*os_ << "  jmp L" << l << endl;
		}
	}
	if (!otherwise)
		*os_ << "  ret" << endl;
	auto v1it = v1.cbegin();
	for (auto it=children.begin();it!=children.end();++it) {
		assert(it->which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(*it));
		if (IsPiece(comp)) {
			*os_ << " L" << *v1it++ << ':' << endl;
			if (!EmitCode(n, comp.children.at(0)))
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
	int l = avail_l_++;
	int p0 = avail_n_++;
	int p1 = avail_n_++;
	int p = avail_n_++;
	*os_ << "  loadi $" << p0 << " 0" << endl
				 << "  loadi $" << p1 << " 1" << endl
				 << "  $" << p << " = ($uniform_variate $" << p0 << " $" << p1 << ')' << endl;
	std::vector<int> v1;
	for (auto it=children.begin();it!=children.end();++it) {
		assert(it->which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(*it));
		if (IsOutcome(comp)) {
			int l1 = avail_l_++;
			int m0 = avail_n_++;
			*os_ << "  loadi $" << m0 << ' ';
			boost::apply_visitor(Printer(os_), comp.children.at(1));
			*os_ << endl;
			int m1 = avail_n_++;
			*os_ << "  $" << m1 << " = (leq $" << p << " $" << m0 << ')' << endl
				 << "  br $" << m1 << " L" << l1 << endl;
			v1.push_back(l1);
		} else {
			cerr << "error: unexpected child of $trial: " << uuid_ << ' ' << id_ << endl;
			return false;
		}
	}
	*os_ << "  ret" << endl;
	auto v1it = v1.cbegin();
	for (auto it=children.begin();it!=children.end();++it) {
		assert(it->which() == cas::kExprIsCompound);
		cas::Compound &comp(boost::get<cas::Compound>(*it));
		if (IsOutcome(comp)) {
			*os_ << " L" << *v1it++ << ':' << endl;
			if (!EmitCode(n, comp.children.at(0)))
				return false;
			*os_ << "  jmp L" << l << endl;
		} else {
			assert(false);
		}
	}
	*os_ << " L" << l << ':' << endl;
	return bool(*os_);
}

bool Context::EmitCode(int n, cas::Expr &sexp)
{
	int w = sexp.which();
	switch (w) {
	case cas::kExprIsString:
		{
			const std::string &s(boost::get<std::string>(sexp));
			if ( (s[0] == '%' || s[0] == '@') && isalpha(s[1]) ) {
				*os_ << "  load $" << n << ' ' << s.c_str() << endl;
			} else {
				*os_ << "  loadi $" << n << ' ' << s.c_str() << endl;
			}
		}
		break;
	case cas::kExprIsCompound:
		{
			cas::Compound &comp(boost::get<cas::Compound>(sexp));
			if (IsAt(comp)) {
				return EmitAt(n, comp.children);
			}
			if (IsLookback(comp)) {
				return EmitLookback(n, comp.children);
			}
			if (IsPiecewise(comp)) {
				return EmitPiecewise(n, comp.children);
			}
			if (IsTrial(comp)) {
				return EmitTrial(n, comp.children);
			}

			size_t len = comp.children.size();
			if (len > 2) {
				cerr << "error: more than 2 arguments for " << comp.keyword << ": "
					 << uuid_ << ' ' << id_ << endl;
				return false;
			}
			if (len < 1) {
				cerr << "error: missing arguments for " << comp.keyword << ": "
					 << uuid_ << ' ' << id_ << endl;
				return false;
			}
			for (auto it=comp.children.begin();it!=comp.children.end();++it) {
				int m = avail_n_++;
				if (!EmitCode(m, *it))
					return false;
				std::unique_ptr<char[]> buf(new char[20]);
				sprintf(buf.get(), "$%d", m);
				*it = buf.get();
			}
			*os_ << "  $" << n << " = ";
			boost::apply_visitor(Printer(os_), sexp);
			*os_ << endl;
		}
		break;
	case cas::kExprIsInteger:
	case cas::kExprIsReal:
		{
			*os_ << "  loadi $" << n << ' ';
			boost::apply_visitor(Printer(os_), sexp);
			*os_ << endl;
		}
		break;
	}
	return true;
}

}
}
}
