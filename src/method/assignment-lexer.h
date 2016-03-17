/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_ASSIGNMENT_LEXER_H_
#define FLINT_METHOD_ASSIGNMENT_LEXER_H_

#include <string>

#include <boost/spirit/include/lex_lexertl.hpp>

#include "lexer.h"

namespace flint {
namespace method {

using namespace boost::spirit;

template<typename TLexer>
struct AssignmentLexer : lex::lexer<TLexer> {

	AssignmentLexer() {
		this->self.add_pattern
			("DIGIT", "[0-9]")
			("SIGN", "[-+]")
			("EXPONENT", "[eE]{SIGN}?{DIGIT}+")
			("FLOAT", "{SIGN}?({DIGIT}*\".\"{DIGIT}+{EXPONENT}?|{DIGIT}+{EXPONENT})")
			;

		case_set_ = "case-set";
		case_ = "case";
		condition_ = "condition";
		eq_ = "eq";
		delay_ = "\"$\"Delay";
		delta_time_ = "\"$\"DeltaTime";

		real = "{FLOAT}";
		integer = "{SIGN}?{DIGIT}+";
		id = "\"%\"[a-zA-Z_][a-zA-Z_0-9:]*";
		keyword = "[$]?[a-zA-Z_][a-zA-Z_0-9]*";

		this->self = lex::token_def<>('\n') | '\r' | '(' | ')' | ' ';
		this->self += case_set_ | case_ | condition_ | eq_ | delay_ | delta_time_;
		this->self += real | integer | id | keyword;
	}

	lex::token_def<std::string> case_set_, case_, condition_, eq_, delay_, delta_time_;
	lex::token_def<std::string> id, keyword;
	lex::token_def<int> integer;
	lex::token_def<flint::lexer::Real> real;
};

}
}

#endif
