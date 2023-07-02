/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_FORMULA_LEXER_H_
#define FLINT_GUI_FORMULA_LEXER_H_

#include <iostream>
#include <memory>

namespace flint {
namespace gui {
namespace formula {

struct Token;
class LexerImpl;

class Lexer {
public:
	Lexer(const char *input, std::ostream &es);

	~Lexer();

	int operator()(Token *token);

private:
	std::unique_ptr<LexerImpl> impl_;
};

}
}
}

#endif
