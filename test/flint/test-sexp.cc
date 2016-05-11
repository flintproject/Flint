/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/sexp.h"

#include <sstream>

#define BOOST_TEST_MODULE test_sexp
#include "test.h"

#include "flint/token.h"

struct F {
	Token t;
	std::unique_ptr<sexp::Expression> expr;
};

struct V : public sexp::Visitor<void> {
	void operator()(const sexp::Identifier &a) {
		oss << a.GetString();
	}

	void operator()(const sexp::Literal &b) {
		oss << std::string(b.token().lexeme, b.token().size);
	}

	void operator()(const sexp::Compound &c) {
		oss << '(';
		bool first = true;
		for (auto &child : c.children()) {
			if (first) {
				first = false;
			} else {
				oss << ' ';
			}
			sexp::ApplyVisitor(*this, *child);
		}
		oss << ')';
	}

	std::string GetString() {
		return oss.str();
	}

	std::ostringstream oss;
};

BOOST_FIXTURE_TEST_SUITE(test_sexp, F)

BOOST_AUTO_TEST_CASE(Visitor) {
	std::vector<std::unique_ptr<sexp::Expression> > xyz1;
	std::vector<std::unique_ptr<sexp::Expression> > yz1;

	t.type = Token::Type::kKeyword;
	t.lexeme = "y";
	t.size = 1;
	yz1.emplace_back(new sexp::Identifier(t));
	t.type = Token::Type::kIdentifier;
	t.lexeme = "%z";
	t.size = 2;
	yz1.emplace_back(new sexp::Identifier(t));
	t.type = Token::Type::kInteger;
	t.lexeme = "1";
	t.size = 1;
	yz1.emplace_back(new sexp::Literal(t));

	t.type = Token::Type::kKeyword;
	t.lexeme = "x";
	t.size = 1;
	xyz1.emplace_back(new sexp::Identifier(t));
	xyz1.emplace_back(new sexp::Compound(std::move(yz1)));
	expr.reset(new sexp::Compound(std::move(xyz1)));
	V v;
	sexp::ApplyVisitor(v, *expr);
	BOOST_CHECK_EQUAL(v.GetString(), "(x (y %z 1))");

	std::ostringstream oss;
	expr->Write(&oss);
	BOOST_CHECK_EQUAL(oss.str(), "(x (y %z 1))");
}

BOOST_AUTO_TEST_SUITE_END()
