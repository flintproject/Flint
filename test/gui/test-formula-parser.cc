/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "gui/formula/parser.h"

#define BOOST_TEST_MODULE test_gui_formula_parser
#include "test.h"

#include "gui/formula/token.h"
#include "gui/formula/tree.h"

using gui::formula::Parser;
using gui::formula::Token;
using gui::formula::Tree;

struct F {
	std::ostringstream es;
	std::unique_ptr<Tree> tree;
};

BOOST_FIXTURE_TEST_SUITE(test_parser, F)

BOOST_AUTO_TEST_CASE(Empty) {
	Parser p0("", es);
	tree.reset(p0());
	BOOST_REQUIRE(!tree);

	Parser p1("   ", es);
	BOOST_REQUIRE(!tree);
}

BOOST_AUTO_TEST_CASE(Parenthesis) {
	Parser p0("(", es);
	tree.reset(p0());
	BOOST_REQUIRE(!tree);

	Parser p1(")", es);
	tree.reset(p1());
	BOOST_REQUIRE(!tree);

	Parser p2("()", es);
	tree.reset(p2());
	BOOST_REQUIRE(!tree);
}

BOOST_AUTO_TEST_CASE(Identifier) {
	Parser p0("x", es);
	tree.reset(p0());
	BOOST_REQUIRE(tree);
	BOOST_CHECK(tree->op == Tree::Op::kCi);
}

BOOST_AUTO_TEST_CASE(Integer) {
	Parser p("+24", es);
	tree.reset(p());
	BOOST_REQUIRE(tree);
	BOOST_CHECK(tree->op == Tree::Op::kCn);
}

BOOST_AUTO_TEST_CASE(Real) {
	Parser p("-1.259", es);
	tree.reset(p());
	BOOST_REQUIRE(tree);
	BOOST_CHECK(tree->op == Tree::Op::kCn);
}

BOOST_AUTO_TEST_CASE(Formula) {
	Parser p("a + 1.5 * (b - min(1, x, y) * c) / 5", es);
	tree.reset(p());
	BOOST_REQUIRE(tree);
	BOOST_CHECK(tree->op == Tree::Op::kPlus);
	BOOST_CHECK_EQUAL(tree->children.size(), 2u);
	BOOST_CHECK(tree->children.at(0)->op == Tree::Op::kCi);
	BOOST_CHECK(tree->children.at(1)->op == Tree::Op::kDivide);
	BOOST_CHECK_EQUAL(tree->children.at(1)->children.size(), 2u);
	BOOST_CHECK(tree->children.at(1)->children.at(0)->op == Tree::Op::kTimes);
	BOOST_CHECK(tree->children.at(1)->children.at(1)->op == Tree::Op::kCn);
	BOOST_CHECK_EQUAL(tree->children.at(1)->children.at(0)->children.size(), 2u);
	BOOST_CHECK(tree->children.at(1)->children.at(0)->children.at(0)->op == Tree::Op::kCn);
	BOOST_CHECK(tree->children.at(1)->children.at(0)->children.at(1)->op == Tree::Op::kMinus);
	BOOST_CHECK_EQUAL(tree->children.at(1)->children.at(0)->children.at(1)->children.size(), 2u);
	BOOST_CHECK(tree->children.at(1)->children.at(0)->children.at(1)->children.at(0)->op == Tree::Op::kCi);
	BOOST_CHECK(tree->children.at(1)->children.at(0)->children.at(1)->children.at(1)->op == Tree::Op::kTimes);
	BOOST_CHECK_EQUAL(tree->children.at(1)->children.at(0)->children.at(1)->children.at(1)->children.size(), 2u);
	BOOST_CHECK(tree->children.at(1)->children.at(0)->children.at(1)->children.at(1)->children.at(0)->op == Tree::Op::kMin);
	BOOST_CHECK(tree->children.at(1)->children.at(0)->children.at(1)->children.at(1)->children.at(1)->op == Tree::Op::kCi);
	BOOST_CHECK_EQUAL(tree->children.at(1)->children.at(0)->children.at(1)->children.at(1)->children.at(0)->children.size(), 3u);
}

BOOST_AUTO_TEST_SUITE_END()
