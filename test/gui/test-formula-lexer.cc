/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "gui/formula/lexer.h"

#define BOOST_TEST_MODULE test_gui_formula_lexer
#include "test.h"

#include "gui/formula/token.h"

using gui::formula::Lexer;
using gui::formula::Token;

struct F {
	std::ostringstream es;
	Token token;
};

#define CHECK_TOKEN(expected_type, expected_size)		\
	do {												\
		BOOST_CHECK(token.type == expected_type);		\
		BOOST_CHECK_EQUAL(token.size, expected_size);	\
	} while (0)

BOOST_FIXTURE_TEST_SUITE(test_lexer, F)

BOOST_AUTO_TEST_CASE(Empty) {
	Lexer t0("", es);
	BOOST_CHECK_EQUAL(t0(&token), 0);

	Lexer t1("   ", es);
	BOOST_CHECK_EQUAL(t1(&token), 0);
}

BOOST_AUTO_TEST_CASE(Parenthesis) {
	Lexer t0("(", es);
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kParenOpen, 1);

	Lexer t1(")", es);
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kParenClose, 1);
}

BOOST_AUTO_TEST_CASE(Identifier) {
	Lexer t0("x", es);
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 1);

	Lexer t1("_0Q", es);
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 3);

	Lexer t2("XY2_", es);
	BOOST_CHECK_EQUAL(t2(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 4);
}

BOOST_AUTO_TEST_CASE(Integer) {
	Lexer t0("0", es);
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 1);

	Lexer t1("+0", es);
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 2);

	Lexer t2("-0", es);
	BOOST_CHECK_EQUAL(t2(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 2);

	Lexer t3("+24", es);
	BOOST_CHECK_EQUAL(t3(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 3);

	Lexer t4("-1230", es);
	BOOST_CHECK_EQUAL(t4(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 5);
}

BOOST_AUTO_TEST_CASE(Real) {
	Lexer t0(".0", es);
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 2);

	Lexer t1("+0.0", es);
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 4);

	Lexer t2("-.0", es);
	BOOST_CHECK_EQUAL(t2(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 3);

	Lexer t3("1.259", es);
	BOOST_CHECK_EQUAL(t3(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 5);

	Lexer t4("-1.044e10", es);
	BOOST_CHECK_EQUAL(t4(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 9);

	Lexer t5("1.044E-7", es);
	BOOST_CHECK_EQUAL(t5(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 8);

	Lexer t6("2e+8", es);
	BOOST_CHECK_EQUAL(t6(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 4);
}

BOOST_AUTO_TEST_CASE(Compound) {
	Lexer t("foo * bar - (baz + 2) / quux", es);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 3);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kStar, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 3);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kMinus, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kParenOpen, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 3);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kPlus, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kNumber, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kParenClose, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kSlash, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 4);
	BOOST_CHECK_EQUAL(t(&token), 0);
}

BOOST_AUTO_TEST_SUITE_END()
