/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SEXP_PARSER_H_
#define FLINT_SEXP_PARSER_H_

#include <memory>

namespace flint {
namespace sexp {
class Expression;

namespace parser {

class Impl;

class Parser {
public:
	explicit Parser(const char *input);

	~Parser();

	int operator()(std::unique_ptr<sexp::Expression> *expp);

private:
	std::unique_ptr<Impl> impl_;
};

}
}
}

#endif
