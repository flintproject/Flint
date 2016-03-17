/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "delay-arg-validator.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "db/statement-driver.h"

using std::cerr;
using std::endl;

using namespace boost::spirit;

namespace flint {
namespace phml {
namespace dav {

struct Sexp;
struct Dexp;

typedef boost::variant<boost::recursive_wrapper<Sexp>, Dexp, std::string> Math;

struct Sexp {
	std::vector<Math> children;
};

struct Dexp {
	std::string arg;
};

}
}
}

BOOST_FUSION_ADAPT_STRUCT(flint::phml::dav::Sexp, (std::vector<flint::phml::dav::Math>, children))
BOOST_FUSION_ADAPT_STRUCT(flint::phml::dav::Dexp, (std::string, arg))

namespace flint {
namespace phml {

namespace {

class Visitor : public boost::static_visitor<bool>
{
public:
	Visitor(sqlite3_int64 rowid, sqlite3 *db)
		: rowid_(rowid),
		  driver_(db,
				  "SELECT max_delay FROM pqs WHERE module_rowid = ? AND name = ?")
	{
	}

	bool operator()(const dav::Sexp &sexp) const
	{
		const std::vector<dav::Math> &c(sexp.children);
		for (const auto &math : c) {
			if (!boost::apply_visitor(*this, math))
				return false;
		}
		return true;
	}

	bool operator()(const dav::Dexp &dexp) const
	{
		int e;
		e = sqlite3_bind_int64(driver_.stmt(), 1, rowid_);
		if (e != SQLITE_OK) {
			cerr << "failed to bind module_rowid: " << e << endl;
			return false;
		}
		const char *arg = dexp.arg.c_str();
		const char *name = arg+1; // skip the leading '%'
		e = sqlite3_bind_text(driver_.stmt(), 2, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind name: " << e << endl;
			return false;
		}
		e = sqlite3_step(driver_.stmt());
		switch (e) {
		case SQLITE_ROW:
			{
				const unsigned char *max_delay = sqlite3_column_text(driver_.stmt(), 0);
				if (!max_delay) {
					cerr << name
						 << " is given as 1st argument of Delay(), but it lacks <max-delay>"
						 << endl;
					return false;
				}
			}
			sqlite3_reset(driver_.stmt());
			return true;
		case SQLITE_DONE:
			cerr << "Delay()'s 1st argument must be a PQ name in the same module: "
				 << name
				 << endl;
			return false;
		default:
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
	}

	bool operator()(const std::string &) const {return true;}

private:
	sqlite3_int64 rowid_;
	db::StatementDriver driver_;
};

template<typename TLexer>
struct DelayArgLexer : lex::lexer<TLexer> {

	DelayArgLexer() {
		constant = "[^()%$ ]+";
		id = "%[^()%$ ]+";
		dollar_delay = "\\$Delay";
		keyword = "\\$[^()%$ ]+";
		whitespace = "[ ]+";

		this->self = lex::token_def<>('(') | ')' | constant | id | dollar_delay | keyword;

		this->self("WS") = whitespace;
	}

	lex::token_def<std::string> constant, id, keyword;
	lex::token_def<> dollar_delay, whitespace;
};

template<typename TIterator, typename TLexer>
struct DelayArgGrammar : qi::grammar<TIterator, dav::Math(), qi::in_state_skipper<TLexer> > {

	template<typename TTokenDef>
	DelayArgGrammar(TTokenDef const &td)
		: DelayArgGrammar::base_type(start)
	{
		using boost::phoenix::at_c;

		start = (dexp | sexp | td.constant | td.id | td.keyword);

		dexp = '(' >> td.dollar_delay
				   >> td.id [at_c<0>(_val) = _1]
				   >> start
				   >> ')' ;

		sexp %= '(' >> +start >> ')';
	}

	qi::rule<TIterator, dav::Math(), qi::in_state_skipper<TLexer> > start;
	qi::rule<TIterator, dav::Dexp(), qi::in_state_skipper<TLexer> > dexp;
	qi::rule<TIterator, dav::Sexp(), qi::in_state_skipper<TLexer> > sexp;
};

class Parser {
public:
	typedef const char * base_iterator_type;
	typedef lex::lexertl::token<base_iterator_type> token_type;
	typedef lex::lexertl::lexer<token_type> lexer_type;
	typedef DelayArgLexer<lexer_type> DAL;
	typedef DAL::iterator_type iterator_type;
	typedef DelayArgGrammar<iterator_type, DAL::lexer_def> DAG;

	explicit Parser(sqlite3 *db)
		: db_(db)
		, tokens_()
		, grammar_(tokens_)
	{
	}

	bool Parse(sqlite3_int64 module_rowid, const char *math) {
		const char *p = math;
		iterator_type it = tokens_.begin(p, math + std::strlen(math));
		iterator_type end = tokens_.end();
		dav::Math ast;
		bool r = qi::phrase_parse(it, end, grammar_, qi::in_state("WS")[tokens_.self], ast);
		if (!r || it != end) {
			cerr << "failed to parse math: " << math << endl;
			return false;
		}
		return boost::apply_visitor(Visitor(module_rowid, db_), ast);
	}

private:
	sqlite3 *db_;
	DAL tokens_;
	DAG grammar_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 2);
	Parser *parser = static_cast<Parser *>(data);
	return (parser->Parse(std::atol(argv[0]), argv[1])) ? 0 : 1;
}

bool IsValid(const char *query, Parser *parser, sqlite3 *db)
{
	char *em;
	int e = sqlite3_exec(db, query, Process, parser, &em);
	switch (e) {
	case SQLITE_OK:
		return true;
	case SQLITE_ABORT: // the callback returns non-zero
		return false;
	default:
		cerr << "failed to exec: " << e
			 << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
}

}

DelayArgValidator::DelayArgValidator(sqlite3 *db)
	: db_(db)
{
}

bool DelayArgValidator::Validate()
{
	std::unique_ptr<Parser> parser(new Parser(db_));
	if (!IsValid("SELECT p.module_rowid, i.math FROM impls AS i"
				 " LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid",
				 parser.get(), db_))
		return false;
	if (!IsValid("SELECT p.module_rowid, e.math FROM extras AS e"
				 " LEFT JOIN pqs AS p ON e.pq_rowid = p.rowid",
				 parser.get(), db_))
		return false;
	return true;
}

}
}
