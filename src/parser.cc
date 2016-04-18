/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/parser.h"

#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <stack>
#include <vector>

#include "flint/sexp.h"
#include "flint/token.h"
#include "flint/tokenizer.h"

namespace flint {
namespace parser {

class Impl {
public:
	Impl(const char *input);

	~Impl();

	int Parse(std::unique_ptr<sexp::Expression> *expp);

private:
	tokenizer::Tokenizer tokenizer_;
	std::stack<std::vector<std::unique_ptr<sexp::Expression> > > stack_;
};

Impl::Impl(const char *input)
	: tokenizer_(input)
{
}

Impl::~Impl() = default;

/*
 * Parse input.
 *
 * Returns 1 in case of success,
 * 0 if end of string is found,
 * -1 otherwise, e.g., an error occurs.
 */
int Impl::Parse(std::unique_ptr<sexp::Expression> *expp)
{
	Token token;
	int r;

 next:
	r = tokenizer_(&token);
	if (r <= 0)
		return r;
	switch (token.type) {
	case Token::Type::kParenthesis:
		{
			if (*token.lexeme == '(') {
				stack_.emplace();
				goto next;
			}
			if (stack_.empty()) {
				std::cerr << "unmatched parenthesis found" << std::endl;
				return -1;
			}
			if (stack_.top().empty()) {
				std::cerr << "null list found" << std::endl;
				return -1;
			}
			std::vector<std::unique_ptr<sexp::Expression> > q;
			stack_.top().swap(q);
			stack_.pop();
			if (stack_.empty()) {
				expp->reset(new sexp::Compound(std::move(q)));
				return 1;
			}
			stack_.top().emplace_back(new sexp::Compound(std::move(q)));
			goto next;
		}
	case Token::Type::kIdentifier:
	case Token::Type::kKeyword:
		if (!stack_.empty()) {
			stack_.top().emplace_back(new sexp::Identifier(token));
			goto next;
		}
		expp->reset(new sexp::Identifier(token));
		return 1;
	case Token::Type::kInteger:
	case Token::Type::kReal:
		if (!stack_.empty()) {
			stack_.top().emplace_back(new sexp::Literal(token));
			goto next;
		}
		expp->reset(new sexp::Literal(token));
		return 1;
	default:
		assert(false);
		return -1;
	}
}


Parser::Parser(const char *input)
	: impl_(new Impl(input))
{
}

Parser::~Parser() = default;

int Parser::operator()(std::unique_ptr<sexp::Expression> *expp)
{
	return impl_->Parse(expp);
}

}
}
