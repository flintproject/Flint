/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_EQUATION_PARSER_H_
#define FLINT_METHOD_EQUATION_PARSER_H_

#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/query.h"
#include "method/helper.hh"

#include "sqlite3.h"

namespace flint {
namespace method {

using namespace boost::spirit;

namespace {

bool IsConditional(const Compound &x)
{
	return x.keyword == "case-set";
}

bool IsConditional(const Expr &x, Compound &y)
{
	if (x.which() != kExprIsCompound) return false;
	y = boost::get<Compound>(x);
	return IsConditional(y);
}

bool IsEquation(const Compound &x)
{
	return x.keyword == "eq" && x.children.size() >= 2;
}

bool IsEquation(const Expr &x, Compound &y)
{
	if (x.which() != kExprIsCompound) return false;
	y = boost::get<Compound>(x);
	return IsEquation(y);
}

void ReportInvalidLeafOfCaseSet(const boost::uuids::uuid &uuid)
{
	std::cerr << "invalid formula found in <case-set>: " << uuid << std::endl;
}

bool TransformConditional(const Compound &c, Expr &lhs, Compound &rhs)
{
	rhs.keyword = "piecewise";
	rhs.children.clear();

	for (const auto &child : c.children) {
		assert(child.which() == kExprIsCompound);
		const Compound &cs = boost::get<Compound>(child);
		assert(cs.keyword == "case");
		Compound r;
		Compound w;
		switch (cs.children.size()) {
		case 1:
			{
				r.keyword = "otherwise";
				r.children.resize(1);

				const Expr &v(cs.children[0]);
				if (IsConditional(v, w)) {
					Compound t;
					if (!TransformConditional(w, lhs, t)) return false;
					r.children[0] = t;
				} else if (IsEquation(v, w)) {
					lhs = w.children[0];
					r.children[0] = w.children[1];
				} else {
					return false;
				}

				rhs.children.push_back(r);
			}
			break;
		case 2:
			{
				r.keyword = "piece";
				r.children.resize(2);

				const Expr &v0(cs.children[0]);
				const Expr &v1(cs.children[1]);
				r.children[1] = v0;
				if (IsConditional(v1, w)) {
					Compound t;
					if (!TransformConditional(w, lhs, t)) return false;
					r.children[0] = t;
				} else if (IsEquation(v1, w)) {
					lhs = w.children[0];
					r.children[0] = w.children[1];
				} else {
					return false;
				}

				rhs.children.push_back(r);
			}
			break;
		default:
			assert(false);
			break;
		}
	}
	return true;
}

/*
 * This class creates and keeps both tokens and grammar objects which
 * construction is expensive in terms of performance.
 */
template<template<typename> class TLexer,
		 template<typename> class TGrammar,
		 typename TInserter>
class Parser {
public:
	typedef const char *base_iterator_type;
	typedef lex::lexertl::token<base_iterator_type> token_type;
	typedef lex::lexertl::lexer<token_type> lexer_type;
	typedef TLexer<lexer_type> RealLexer;
	typedef TGrammar<typename RealLexer::iterator_type> RealGrammar;

	explicit Parser(sqlite3 *db)
		: tokens_()
		, grammar_(tokens_)
		, inserter_(db)
	{
	}

	int Parse(const boost::uuids::uuid &uuid, const char *math) {
		base_iterator_type it = math;
		base_iterator_type eit = math + std::strlen(math);
		Compound statement;
		bool r = lex::tokenize_and_parse(it, eit, tokens_, grammar_, statement);
		if (!r || it != eit) {
			std::cerr << "failed to parse input: " << it << std::endl;
			return 1;
		}
		return ProcessUuidAndStatement(uuid, statement);
	}

private:
	int ProcessUuidAndStatement(const boost::uuids::uuid &uuid, Compound &statement)
	{
		if (IsConditional(statement)) {
			Expr lhs;
			Compound rhs;
			if (!TransformConditional(statement, lhs, rhs)) {
				ReportInvalidLeafOfCaseSet(uuid);
				return 1;
			}
			if (!inserter_.PrintAndInsert(uuid, lhs, rhs))
				return 1;
		} else if (IsEquation(statement)) {
			if (!inserter_.PrintAndInsert(uuid,
										  statement.children[0],
										  statement.children[1]))
				return 1;
		} else {
			assert(false);
			return 1;
		}
		return 0;
	}

	RealLexer tokens_;
	RealGrammar grammar_;
	TInserter inserter_;
};

template<template<typename> class TLexer,
		 template<typename> class TGrammar,
		 typename TInserter>
int Process(void *data, int argc, char **argv, char **names)
{
	typedef Parser<TLexer, TGrammar, TInserter> RealParser;

	RealParser *parser = static_cast<RealParser *>(data);
	(void)names;
	assert(argc == 2);
	assert(argv[0]);
	boost::uuids::uuid u;
	memcpy(&u, argv[0], u.size());
	return parser->Parse(u, argv[1]);
}

}

template<int TNol,
		 template<typename> class TLexer,
		 template<typename> class TGrammar,
		 typename TInserter>
bool Parse(sqlite3 *db, const char *input, sqlite3 *output)
{
	if (!SaveNol(TNol, output))
		return false;
	if (!CreateAsts(output))
		return false;

	Parser<TLexer, TGrammar, TInserter> parser(output);

	std::ostringstream oss;
	oss << "SELECT * FROM " << input;
	std::string query_i = oss.str();

	char *em;
	int e;
	e = sqlite3_exec(db, query_i.c_str(), &Process<TLexer, TGrammar, TInserter>, &parser, &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			std::cerr << "failed to select " << input
					  << ": " << e << ": " << em << std::endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

}
}

#endif
