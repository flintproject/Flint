/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/sexp/tokenizer.h"

#define BOOST_TEST_MODULE test_tokenizer
#include "test.h"

#include "flint/sexp/token.h"

using sexp::Token;
using sexp::tokenizer::Tokenizer;

struct F {
	Token token;
};

#define CHECK_TOKEN(expected_type, expected_size)		\
	do {												\
		BOOST_CHECK(token.type == expected_type);		\
		BOOST_CHECK_EQUAL(token.size, expected_size);	\
	} while (0)

BOOST_FIXTURE_TEST_SUITE(test_tokenizer, F)

BOOST_AUTO_TEST_CASE(Empty) {
	Tokenizer t0("");
	BOOST_CHECK_EQUAL(t0(&token), 0);

	Tokenizer t1("   ");
	BOOST_CHECK_EQUAL(t1(&token), 0);
}

BOOST_AUTO_TEST_CASE(Parenthesis) {
	Tokenizer t0("(");
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kParenthesis, 1);

	Tokenizer t1(")");
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kParenthesis, 1);
}

BOOST_AUTO_TEST_CASE(Identifier) {
	Tokenizer t0("%x");
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 2);

	Tokenizer t1("@_0Q");
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 4);

	Tokenizer t2("@XYZ:p#100");
	BOOST_CHECK_EQUAL(t2(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 10);
}

BOOST_AUTO_TEST_CASE(Keyword) {
	Tokenizer t0("x");
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kKeyword, 1);

	Tokenizer t1("_f00_");
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kKeyword, 5);

	Tokenizer t2("$Flint");
	BOOST_CHECK_EQUAL(t2(&token), 1);
	CHECK_TOKEN(Token::Type::kKeyword, 6);

	Tokenizer t3("case-set");
	BOOST_CHECK_EQUAL(t3(&token), 1);
	CHECK_TOKEN(Token::Type::kKeyword, 8);
}

BOOST_AUTO_TEST_CASE(Integer) {
	Tokenizer t0("0");
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kInteger, 1);

	Tokenizer t1("+0");
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kInteger, 2);

	Tokenizer t2("-0");
	BOOST_CHECK_EQUAL(t2(&token), 1);
	CHECK_TOKEN(Token::Type::kInteger, 2);

	Tokenizer t3("+24");
	BOOST_CHECK_EQUAL(t3(&token), 1);
	CHECK_TOKEN(Token::Type::kInteger, 3);

	Tokenizer t4("-1230");
	BOOST_CHECK_EQUAL(t4(&token), 1);
	CHECK_TOKEN(Token::Type::kInteger, 5);
}

BOOST_AUTO_TEST_CASE(Real) {
	Tokenizer t0(".0");
	BOOST_CHECK_EQUAL(t0(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 2);

	Tokenizer t1("+0.0");
	BOOST_CHECK_EQUAL(t1(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 4);

	Tokenizer t2("-.0");
	BOOST_CHECK_EQUAL(t2(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 3);

	Tokenizer t3("1.259");
	BOOST_CHECK_EQUAL(t3(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 5);

	Tokenizer t4("-1.044e10");
	BOOST_CHECK_EQUAL(t4(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 9);

	Tokenizer t5("1.044E-7");
	BOOST_CHECK_EQUAL(t5(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 8);

	Tokenizer t6("2e+8");
	BOOST_CHECK_EQUAL(t6(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 4);

	Tokenizer t7("7."); // ending with "." is valid
	BOOST_CHECK_EQUAL(t7(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 2);
}

BOOST_AUTO_TEST_CASE(Compound) {
	Tokenizer t("(hello (world %x1 2.3) 4)");
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kParenthesis, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kKeyword, 5);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kParenthesis, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kKeyword, 5);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kIdentifier, 3);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kReal, 3);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kParenthesis, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kInteger, 1);
	BOOST_CHECK_EQUAL(t(&token), 1);
	CHECK_TOKEN(Token::Type::kParenthesis, 1);
	BOOST_CHECK_EQUAL(t(&token), 0);
}

BOOST_AUTO_TEST_SUITE_END()
