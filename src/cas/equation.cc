/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>

#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "cas/dimension.h"
#include "cas/helper.h"

using std::cerr;
using std::endl;

using namespace boost::spirit;

namespace flint {
namespace cas {

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

bool IsBoundVariable(const Expr &expr)
{
	if (expr.which() == kExprIsCompound) {
		const Compound &c = boost::get<Compound>(expr);
		if (c.keyword == "bvar") {
			if (c.children.size() == 1) {
				const Expr &e0 = c.children[0];
				if (e0.which() == kExprIsIdentifier) {
					const std::string &s0 = boost::get<Identifier>(e0).name;
					if (s0 == "%time")
						return true;
				}
				cerr << "unsupported bound variable of <diff>" << endl;
				return false;
			}
		}
	}
	cerr << "unsupported form of <diff>'s 1st argument" << endl;
	return false;
}

void ReportInvalidLeafOfCaseSet(const boost::uuids::uuid &uuid)
{
	cerr << "invalid formula found in <case-set>: " << uuid << endl;
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

template<typename TLexer>
struct Lexer : lex::lexer<TLexer> {

	Lexer() {
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

void RewriteDelayParam(Compound &x, const Expr &expr)
{
	Compound c;
	c.keyword = "minus";
	c.children.push_back(Identifier("%time"));
	c.children.push_back(expr);
	x.children.push_back(c);
}

template<typename TIterator>
struct Grammar : qi::grammar<TIterator, Compound()> {

	template<typename TTokenDef>
	Grammar(TTokenDef const &td)
	: Grammar::base_type(statement)
	{
		using boost::phoenix::at_c;
		using boost::phoenix::push_back;
		using boost::phoenix::val;

		statement = equation | conditional;

		equation = '(' >> td.eq_ [at_c<0>(_val) = _1]
					   >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
					   >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
					   >> ')';

		conditional = '(' >> td.case_set_
						  >> cseq
						  >> ')';

		cseq = +(' ' >> cexp [push_back(_val, _1)]);

		cexp = ('(' >> td.case_ [at_c<0>(_val) = _1]
				>> ' ' >> '(' >> td.condition_
				>> ' ' >> expr [push_back(at_c<1>(_val), _1)]
				>> ')'
				>> ' '
				>> statement [push_back(at_c<1>(_val), _1)]
				>> ')')
			| ('(' >> td.case_ [at_c<0>(_val) = _1]
			   >> ' '
			   >> statement [push_back(at_c<1>(_val), _1)]
			   >> ')');

		expr = delay_expr
			| delta_time_expr
			| eq_expr
			| general_expr
			| td.real
			| td.integer
			| td.id
			| td.keyword;

		delay_expr = '(' >> td.delay_ [at_c<0>(_val) = val("$lookback")]
						 >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
						 >> ' ' >> expr [bind(&RewriteDelayParam, _val, _1)]
						 >> ')';

		delta_time_expr = '(' >> td.delta_time_
							  >> ' ' >> td.id [bind(&RewriteDeltaTime, _val, _1)]
							  >> ')';

		eq_expr = '(' >> td.eq_
					  >> seq1
					  >> ')';

		general_expr = '(' >> td.keyword
						   >> seq0
						   >> ')';

		seq0 = *rest;

		seq1 = +rest;

		rest = ' ' >> expr [_val = _1];
	}

	qi::rule<TIterator, Expr()> expr, rest;
	qi::rule<TIterator, Compound()> statement, equation, conditional, cexp, delay_expr, delta_time_expr, eq_expr, general_expr;
	qi::rule<TIterator, std::deque<Expr>()> cseq, seq0, seq1;
};

/*
 * This class creates and keeps both tokens and grammar objects which
 * construction is expensive in terms of performance.
 */
class Parser {
public:
	typedef const char *base_iterator_type;
	typedef lex::lexertl::token<base_iterator_type> token_type;
	typedef lex::lexertl::lexer<token_type> lexer_type;
	typedef Lexer<lexer_type> RealLexer;
	typedef Grammar<RealLexer::iterator_type> RealGrammar;

	explicit Parser(System *output)
		: tokens_()
		, grammar_(tokens_)
		, da_()
		, output_(output)
	{
	}

	bool Load(sqlite3 *db) {
		return da_.Load(db);
	}

	int Parse(const boost::uuids::uuid &uuid, const char *math) {
		base_iterator_type it = math;
		base_iterator_type eit = math + std::strlen(math);
		Compound statement;
		bool r = lex::tokenize_and_parse(it, eit, tokens_, grammar_, statement);
		if (!r || it != eit) {
			cerr << "failed to parse equation: " << it << endl;
			return 1;
		}
		return ProcessUuidAndStatement(uuid, statement);
	}

private:
	int ProcessUuidAndStatement(const boost::uuids::uuid &uuid, Compound &statement)
	{
		Expr lhs, rhs;
		if (IsConditional(statement)) {
			Compound c;
			if (!TransformConditional(statement, lhs, c)) {
				ReportInvalidLeafOfCaseSet(uuid);
				return 1;
			}
			rhs = c;
		} else if (IsEquation(statement)) {
			lhs = statement.children[0];
			rhs = statement.children[1];
		} else {
			assert(false);
			return 1;
		}
		int lhs_col, lhs_row;
		if (!da_.Analyse(uuid, &lhs, &lhs_col, &lhs_row))
			return 1;
		int rhs_col, rhs_row;
		if (!da_.Analyse(uuid, &rhs, &rhs_col, &rhs_row))
			return 1;
		if (lhs_col != rhs_col) {
			cerr << "col mismatch between RHS and LHS" << endl;
			return 1;
		}
		if (lhs_row != rhs_row) {
			cerr << "row mismatch between RHS and LHS" << endl;
			return 1;
		}
		if (lhs.which() == kExprIsCompound) {
			const Compound &c = boost::get<Compound>(lhs);
			return AcceptCompound(uuid, c, lhs_col, lhs_row, rhs) ? 0 : 1;
		} else if (lhs.which() == kExprIsIdentifier) {
			output_->Add(uuid, Def(boost::get<Identifier>(lhs).name, lhs_col, lhs_row, rhs));
		} else {
			cerr << "unsupported form of equation" << endl; // TODO
			return 1;
		}
		return 0;
	}

	bool AcceptCompound(const boost::uuids::uuid &uuid, const Compound &c,
						int col, int row,
						Expr rhs, Expr mass = 1)
	{
		if (c.keyword == "diff") {
			assert(c.children.size() == 2);
			if (!IsBoundVariable(c.children[0]))
				return false;
			const Expr &e1 = c.children[1];
			if (e1.which() == kExprIsIdentifier) {
				output_->Add(uuid, Ode(boost::get<Identifier>(e1).name,
									   col, row, rhs, mass));
				return true;
			}
		} else if (c.keyword == "times") {
			if (c.children.size() == 2) {
				const Expr &e1 = c.children[1];
				if (e1.which() == kExprIsCompound)
					return AcceptCompound(uuid, boost::get<Compound>(e1),
										  col, row, rhs, c.children[0]);
			}
		}
		cerr << "unsupported form of equation's LHS" << endl; // TODO
		return false;
	}

	RealLexer tokens_;
	RealGrammar grammar_;
	DimensionAnalyzer da_;
	System *output_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	Parser *parser = static_cast<Parser *>(data);
	(void)names;
	assert(argc == 2);
	assert(argv[0]);
	boost::uuids::uuid u;
	std::memcpy(&u, argv[0], u.size());
	return parser->Parse(u, argv[1]);
}

}

bool AnnotateEquations(sqlite3 *db, const char *input, System *output)
{
	std::unique_ptr<Parser> parser(new Parser(output));
	if (!parser->Load(db))
		return false;

	std::ostringstream oss;
	oss << "SELECT * FROM " << input;
	std::string query = oss.str();

	char *em;
	int e = sqlite3_exec(db, query.c_str(), Process, parser.get(), &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			cerr << "failed to select " << input
				 << ": " << e << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

}
}
