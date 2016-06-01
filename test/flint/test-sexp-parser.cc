/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/sexp/parser.h"

#define BOOST_TEST_MODULE test_parser
#include "test.h"

#include "flint/sexp.h"
#include "flint/sexp/token.h"

using sexp::Token;
using sexp::parser::Parser;

struct F {
	std::unique_ptr<sexp::Expression> exp;
};

#define CHECK_IDENTIFIER(token_type) do {								\
		BOOST_REQUIRE(exp->type() == sexp::Expression::Type::kIdentifier); \
		BOOST_REQUIRE(static_cast<sexp::Identifier *>(exp.get())->token().type == token_type); \
	} while (0)

#define CHECK_LITERAL(token_type) do {									\
		BOOST_REQUIRE(exp->type() == sexp::Expression::Type::kLiteral); \
		BOOST_REQUIRE(static_cast<sexp::Literal *>(exp.get())->token().type == token_type); \
	} while (0)

#define CHECK_COMPOUND(expected_size) do {								\
		BOOST_REQUIRE(exp->type() == sexp::Expression::Type::kCompound); \
		BOOST_REQUIRE_EQUAL(static_cast<sexp::Compound *>(exp.get())->GetSize(), \
							expected_size);								\
	} while (0)

BOOST_FIXTURE_TEST_SUITE(test_parser, F)

BOOST_AUTO_TEST_CASE(Empty) {
	Parser p0("");
	BOOST_REQUIRE_EQUAL(p0(&exp), 0);

	Parser p1("   ");
	BOOST_REQUIRE_EQUAL(p1(&exp), 0);
}

BOOST_AUTO_TEST_CASE(Parenthesis) {
	Parser p0("(");
	BOOST_REQUIRE_EQUAL(p0(&exp), 0);

	Parser p1(")");
	BOOST_REQUIRE_EQUAL(p1(&exp), -1);

	Parser p2("()");
	BOOST_REQUIRE_EQUAL(p2(&exp), -1);
}

BOOST_AUTO_TEST_CASE(Identifier) {
	Parser p0("%x");
	BOOST_REQUIRE_EQUAL(p0(&exp), 1);
	CHECK_IDENTIFIER(Token::Type::kIdentifier);
}

BOOST_AUTO_TEST_CASE(Keyword) {
	Parser p0("x");
	BOOST_REQUIRE_EQUAL(p0(&exp), 1);
	CHECK_IDENTIFIER(Token::Type::kKeyword);
}

BOOST_AUTO_TEST_CASE(Integer) {
	Parser p("+24");
	BOOST_REQUIRE_EQUAL(p(&exp), 1);
	CHECK_LITERAL(Token::Type::kInteger);
}

BOOST_AUTO_TEST_CASE(Real) {
	Parser p("1.259");
	BOOST_REQUIRE_EQUAL(p(&exp), 1);
	CHECK_LITERAL(Token::Type::kReal);
}

BOOST_AUTO_TEST_CASE(Compound) {
	Parser p0("(hello world)");
	BOOST_REQUIRE_EQUAL(p0(&exp), 1);
	CHECK_COMPOUND(2u);

	Parser p1("(hello (world %x1 2.3) 4)");
	BOOST_REQUIRE_EQUAL(p1(&exp), 1);
	CHECK_COMPOUND(3u);

	Parser p2("(hello (world %x1 2.3) 4 (more (and more longer)))");
	BOOST_REQUIRE_EQUAL(p2(&exp), 1);
	CHECK_COMPOUND(4u);
}

BOOST_AUTO_TEST_SUITE_END()
