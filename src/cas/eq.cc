/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas.h"

namespace flint {
namespace cas {

Eq::Eq(const std::string &name, int col, int row, const Expr &rhs)
	: name_(name)
	, col_(col)
	, row_(row)
	, rhs_(rhs)
{
}

Eq::~Eq() = default;

const std::string &Eq::name() const
{
	return name_;
}

int Eq::col() const
{
	return col_;
}

int Eq::row() const
{
	return row_;
}

const Expr &Eq::rhs() const
{
	return rhs_;
}

Ode::Ode(const std::string &name, int col, int row, const Expr &rhs, const Expr &mass)
	: Eq(name, col, row, rhs)
	, mass_(mass)
{
}

Expr &Ode::mass()
{
	return mass_;
}

const Expr &Ode::mass() const
{
	return mass_;
}

Def::Def(const std::string &name, int col, int row, const Expr &rhs)
	: Eq(name, col, row, rhs)
{
}

}
}
