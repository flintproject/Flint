/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "tac.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <iostream>
#include <memory>
#include <string>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "cas.h"
#include "cas/dimension.h"
#include "compiler/tac/context.h"
#include "db/helper.h"
#include "db/query.h"
#include "db/tac-inserter.h"
#include "lexer.h"

using std::memcpy;

using namespace boost::spirit;

namespace flint {
namespace compiler {
namespace tac {
namespace {

/*
 * Precondition: given c's keyword is already set
 */
void ReduceL(cas::Compound &c, std::deque<cas::Expr> &children)
{
	size_t len = children.size();
	assert(len >= 2);
	if (len == 2) {
		c.children.swap(children);
		return;
	}
	c.children.push_back(children.back());
	children.pop_back();
	cas::Compound c0;
	c0.keyword = c.keyword;
	ReduceL(c0, children);
	c.children.push_front(c0);
}

/*
 * Precondition: given c's keyword is already set
 */
void ReduceR(cas::Compound &c, std::deque<cas::Expr> &children)
{
	size_t len = children.size();
	assert(len >= 2);
	if (len == 2) {
		c.children.swap(children);
		return;
	}
	c.children.push_front(children.front());
	children.pop_front();
	cas::Compound c1;
	c1.keyword = c.keyword;
	ReduceR(c1, children);
	c.children.push_back(c1);
}

void Negate(cas::Compound &c, cas::Expr &lexp)
{
	assert(lexp.which() == cas::kExprIsCompound);
	cas::Compound &lcomp(boost::get<cas::Compound>(lexp));
	const std::string &s(lcomp.keyword);

	// in case of compound expression
	if (s == "and") {
		assert(lcomp.children.size() == 2);
		cas::Compound c0;
		cas::Compound c1;
		Negate(c0, lcomp.children.at(0));
		Negate(c1, lcomp.children.at(1));
		c.keyword = "or";
		c.children.push_back(c0);
		c.children.push_back(c1);
		return;
	}
	if (s == "or") {
		assert(lcomp.children.size() == 2);
		cas::Compound c0;
		cas::Compound c1;
		Negate(c0, lcomp.children.at(0));
		Negate(c1, lcomp.children.at(1));
		c.keyword = "and";
		c.children.push_back(c0);
		c.children.push_back(c1);
		return;
	}
	if (s == "not") {
		c = lcomp;
		return;
	}

	// in case of atomic expression
	if (s == "eq") {
		lcomp.keyword = "neq";
		c = lcomp;
		return;
	}
	if (s == "geq") {
		lcomp.keyword = "lt";
		c = lcomp;
		return;
	}
	if (s == "gt") {
		lcomp.keyword = "leq";
		c = lcomp;
		return;
	}
	if (s == "leq") {
		lcomp.keyword = "gt";
		c = lcomp;
		return;
	}
	if (s == "lt") {
		lcomp.keyword = "geq";
		c = lcomp;
		return;
	}
	if (s == "neq") {
		lcomp.keyword = "eq";
		c = lcomp;
		return;
	}

	assert(false); // FIXME
}

void Mean(cas::Compound &c, std::deque<cas::Expr> &children)
{
	size_t len = children.size();
	assert(len >= 2);
	c.keyword = "divide";
	cas::Compound c1;
	c1.keyword = "plus";
	ReduceR(c1, children);
	c.children.push_back(c1);
	c.children.push_back((int)len);
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

		and_ = "and";
		eq_ = "eq";
		geq_ = "geq";
		gt_ = "gt";
		leq_ = "leq";
		log_ = "log";
		logbase_ = "logbase";
		lt_ = "lt";
		max_ = "max";
		mean_ = "mean";
		min_ = "min";
		neq_ = "neq";
		not_ = "not";
		or_ = "or";
		otherwise_ = "otherwise";
		piece_ = "piece";
		piecewise_ = "piecewise";
		plus_ = "plus";
		times_ = "times";
		xor_ = "xor";
		uniform_variate_ = "$uniform_variate";

		real = "{FLOAT}";
		integer = "{SIGN}?{DIGIT}+";
		id = "[%@][a-zA-Z_][a-zA-Z_0-9:#]*";
		keyword = "[$]?[a-zA-Z_][a-zA-Z_0-9]*";

		this->self = lex::token_def<>('\n') | '\r' | '(' | ')' | ' ';
		this->self += and_ | eq_ | geq_ | gt_ | leq_ | log_ | logbase_;
		this->self += lt_ | max_ | mean_ | min_ | neq_ | not_ | or_;
		this->self += otherwise_ | piece_ | piecewise_ | plus_ | times_ | xor_;
		this->self += uniform_variate_;
		this->self += real | integer | id | keyword;
	}

	lex::token_def<std::string> and_, eq_, geq_, gt_, leq_, log_, logbase_;
	lex::token_def<std::string> lt_, max_, mean_, min_, neq_, not_, or_;
	lex::token_def<std::string> otherwise_, piece_, piecewise_, plus_, times_, xor_;
	lex::token_def<std::string> uniform_variate_;
	lex::token_def<std::string> id, keyword;
	lex::token_def<int> integer;
	lex::token_def<flint::lexer::Real> real;
};

template<typename TIterator>
struct Grammar : qi::grammar<TIterator, cas::Expr()> {

	template<typename TTokenDef>
	Grammar(TTokenDef const &td)
	: Grammar::base_type(expr)
	{
		using boost::phoenix::at_c;
		using boost::phoenix::push_back;
		using boost::phoenix::val;

		expr %= td.real
			| td.integer
			| td.id
			| td.keyword
			| '(' >> compound >> ')';

		compound = td.log_ [at_c<0>(_val) = _1] >> ' '
												>> '(' >> td.logbase_ >> ' ' >> expr [push_back(at_c<1>(_val), _1)] >> ')'
												>> ' ' >> expr [push_back(at_c<1>(_val), _1)]
			| td.log_ [at_c<0>(_val) = val("log10")] >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
			| td.piecewise_ [at_c<0>(_val) = _1] >> pseq1 [at_c<1>(_val) = _1]
			| td.max_ [at_c<0>(_val) = _1] >> seq1 [bind(&ReduceR, _val, _1)]
			| td.mean_ >> seq1 [bind(&Mean, _val, _1)]
			| td.min_ [at_c<0>(_val) = _1] >> seq1 [bind(&ReduceR, _val, _1)]
			| td.plus_ [at_c<0>(_val) = _1] >> seq1 [bind(&ReduceR, _val, _1)]
			| td.times_ [at_c<0>(_val) = _1] >> seq1 [bind(&ReduceR, _val, _1)]
			| td.uniform_variate_ [at_c<0>(_val) = _1]
						   >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
						   >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
						   >> ' ' >> expr
						   >> ' ' >> expr
			| td.keyword [at_c<0>(_val) = _1] >> seq0 [at_c<1>(_val) = _1];

		pseq1 = +prest;

		prest = ' ' >> pexp [_val = _1];

		pexp = '(' >> pcomp [_val = _1] >> ')';

		pcomp = td.piece_ [at_c<0>(_val) = _1] >> ' ' >> expr [push_back(at_c<1>(_val), _1)] >> ' ' >> lexp [push_back(at_c<1>(_val), _1)]
			| td.otherwise_ [at_c<0>(_val) = _1] >> ' ' >> expr [push_back(at_c<1>(_val), _1)];

		lexp = '(' >> lcomp [_val = _1] >> ')';

		lcomp = td.and_ [at_c<0>(_val) = _1] >> lseq1 [bind(&ReduceL, _val, _1)]
			| td.or_ [at_c<0>(_val) = _1] >> lseq1 [bind(&ReduceL, _val, _1)]
			| td.xor_ [at_c<0>(_val) = val("neq")] >> lseq1 [bind(&ReduceL, _val, _1)] // logical XOR can be considered as NEQ
			| td.not_ >> ' ' >> lexp [bind(&Negate, _val, _1)]
			| (td.eq_ | td.geq_ | td.gt_ | td.leq_ | td.lt_ | td.neq_) [at_c<0>(_val) = _1] >> ' ' >> expr [push_back(at_c<1>(_val), _1)] >> ' ' >> expr [push_back(at_c<1>(_val), _1)];

		lseq1 = +lrest;

		lrest = ' ' >> lexp [_val = _1];

		seq0 = *rest;

		seq1 = +rest;

		rest = ' ' >> expr [_val = _1];
	}

	qi::rule<TIterator, cas::Expr()> expr, pexp, lexp, rest, prest, lrest;
	qi::rule<TIterator, cas::Compound()> compound, pcomp, lcomp;
	qi::rule<TIterator, std::deque<cas::Expr>()> seq0, seq1, pseq1, lseq1;
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

	Parser(const cas::DimensionAnalyzer *da, sqlite3 *db)
		: tokens_()
		, grammar_(tokens_)
		, da_(da)
		, inserter_(db)
	{
	}

	int Parse(const boost::uuids::uuid &uuid, const char *name, const char *math) {
		base_iterator_type it = math;
		base_iterator_type eit = math + std::strlen(math);
		cas::Expr expr;
		bool r = lex::tokenize_and_parse(it, eit, tokens_, grammar_, expr);
		if (!r || it != eit) {
			std::cerr << "failed to parse expression: " << it << std::endl;
			return 1;
		}
		int col, row;
		if (!da_->Analyse(uuid, &expr, &col, &row))
			return 1;
		std::ostringstream oss;
		std::unique_ptr<Context> context(new Context(uuid, name, &oss));
		if (!context->EmitCode(expr))
			return 1;
		int noir = context->get_ir();
		int nod = context->get_fr();
		std::string body = oss.str();
		return (inserter_.Insert(uuid, name, noir, nod, body.c_str())) ? 0 : 1;
	}

private:
	RealLexer tokens_;
	RealGrammar grammar_;
	const cas::DimensionAnalyzer *da_;
	db::TacInserter inserter_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	Parser *parser = static_cast<Parser *>(data);
	assert(argc == 3);
	assert(argv[0]);
	boost::uuids::uuid uuid;
	memcpy(&uuid, argv[0], uuid.size());
	int r = parser->Parse(uuid, argv[1], argv[2]);
	if (r) // aborting ...
		std::cerr << " in " << uuid << std::endl;
	return r;
}

}

bool Tac(const cas::DimensionAnalyzer *da, sqlite3 *db)
{
	if (!BeginTransaction(db))
		return false;
	if (!CreateTable(db, "tacs", TACS_SCHEMA))
		return false;

	std::unique_ptr<Parser> parser(new Parser(da, db));
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT * FROM sorts", Process, parser.get(), &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			std::cerr << "failed to enumerate sorts: " << e
				 << ": " << em << std::endl;
		sqlite3_free(em);
		return false;
	}
	return CommitTransaction(db);
}

}
}
}
