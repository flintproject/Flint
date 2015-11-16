/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method/printer.h"

namespace flint {
namespace method {

Printer::Printer(std::ostream *os)
	: os_(os)
{}

void Printer::operator()(const Compound &c) const
{
	os_->put('(');
	*os_ << c.keyword;
	for (const auto &expr : c.children) {
		os_->put(' ');
		boost::apply_visitor(*this, expr);
	}
	os_->put(')');
}

void Printer::operator()(const std::string &s) const
{
	*os_ << s;
}

void Printer::operator()(int i) const
{
	*os_ << i;
}

void Printer::operator()(const flint::lexer::Real &r) const
{
	*os_ << r.lexeme;
}

}
}
