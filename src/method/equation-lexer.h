/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_EQUATION_LEXER_H_
#define FLINT_METHOD_EQUATION_LEXER_H_

#include <string>

#include <boost/spirit/include/lex_lexertl.hpp>

#include "lexer.h"

namespace flint {
namespace method {

using namespace boost::spirit;

template<typename TLexer>
struct EquationLexer : lex::lexer<TLexer> {

	EquationLexer() {
		this->self.add_pattern
			("SIGN", "[-+]")
			("EXPONENT", "[eE]{SIGN}?\\d+")
			("FLOAT", "(\".\"\\d+|\\d+\".\"\\d*){EXPONENT}?|\\d+{EXPONENT}")
			;

		case_set_ = "case-set";
		case_ = "case";
		condition_ = "condition";
		diff_ = "diff";
		eq_ = "eq";
		delay_ = "\"$\"Delay";
		delta_time_ = "\"$\"DeltaTime";

		real = "{SIGN}?{FLOAT}";
		rational = "{SIGN}?(0|[1-9]\\d*)\"/\"[1-9]\\d*";
		integer = "{SIGN}?\\d+";
		id = "\"%\"[a-zA-Z_][a-zA-Z_0-9:]*";
		keyword = "[$]?[a-zA-Z_]\\w*";

		this->self = lex::token_def<>('\n') | '\r' | '(' | ')' | ' ';
		this->self += case_set_ | case_ | condition_ | diff_ | eq_ | delay_ | delta_time_;
		this->self += real | rational | integer | id | keyword;
	}

	lex::token_def<std::string> case_set_, case_, condition_, eq_, diff_, delay_, delta_time_;
	lex::token_def<std::string> id, keyword;
	lex::token_def<int> integer;
	lex::token_def<flint::lexer::Rational> rational;
	lex::token_def<flint::lexer::Real> real;
};

}
}

#endif
