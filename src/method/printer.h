/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_PRINTER_H_
#define FLINT_METHOD_PRINTER_H_

#include <iostream>
#include <string>

#include "method.h"

namespace flint {
namespace method {

class Printer : public boost::static_visitor<> {
public:
	explicit Printer(std::ostream *os);

	void operator()(const Compound &c) const;

	void operator()(const std::string &s) const;

	void operator()(int i) const;

	void operator()(const flint::lexer::Real &r) const;

private:
	std::ostream *os_;
};

}
}

#endif
