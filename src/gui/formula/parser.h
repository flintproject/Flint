/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_FORMULA_PARSER_H_
#define FLINT_GUI_FORMULA_PARSER_H_

#include <iostream>
#include <memory>

namespace flint {
namespace gui {
namespace formula {

class ParserImpl;
struct Tree;

class Parser {
public:
	Parser(const char *input, std::ostream &es);

	~Parser();

	Tree *operator()();

private:
	std::unique_ptr<ParserImpl> impl_;
};

}
}
}

#endif
