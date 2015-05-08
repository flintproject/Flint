/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>
#include <boost/variant/recursive_variant.hpp>

using std::cerr;
using std::endl;
using std::putchar;
using std::printf;

using namespace boost::spirit;

struct Compound;

enum {
	kExprIsCompound,
	kExprIsString,
	kExprIsInt,
	kExprIsDouble
};

typedef boost::variant<boost::recursive_wrapper<Compound>,
					   std::string,
					   int,
					   double
					   > Expr;

struct Compound {
	std::string keyword;
	std::vector<Expr> children;
};

BOOST_FUSION_ADAPT_STRUCT(Compound,
						  (std::string, keyword)
						  (std::vector<Expr>, children))

struct Line {
	std::string uuid;
	Compound statement;
};

BOOST_FUSION_ADAPT_STRUCT(Line,
						  (std::string, uuid)
						  (Compound, statement))

namespace {

class Printer : public boost::static_visitor<> {
public:
	void operator()(const Compound &c) const {
		printf("(%s", c.keyword.c_str());
		std::vector<Expr>::const_iterator bit = c.children.begin();
		std::vector<Expr>::const_iterator eit = c.children.end();
		for (std::vector<Expr>::const_iterator it=bit;it!=eit;++it) {
			putchar(' ');
			boost::apply_visitor(*this, *it);
		}
		putchar(')');
	}

	void operator()(const std::string &s) const {
		printf("%s", s.c_str());
	}

	void operator()(int i) const {
		printf("%d", i);
	}

	void operator()(double d) const {
		printf("%g", d);
	}
};

class VariantPrinter : public boost::static_visitor<> {
public:
	VariantPrinter(int k, int n)
		: k_(k)
		, n_(n)
	{}

	void operator()(const Compound &c) const {
		printf("(%s", c.keyword.c_str());
		std::vector<Expr>::const_iterator bit = c.children.begin();
		std::vector<Expr>::const_iterator eit = c.children.end();
		for (std::vector<Expr>::const_iterator it=bit;it!=eit;++it) {
			putchar(' ');
			boost::apply_visitor(*this, *it);
		}
		putchar(')');
	}

	void operator()(const std::string &s) const {
		if (s[0] != '%') {
			printf("%s", s.c_str());
			return;
		}
		if (s == "%time") {
			if (n_ == 1) {
				printf("(plus %%time @dt)");
			} else {
				printf("(plus %%time (divide @dt %d))", n_);
			}
			return;
		}
		printf("%s#%d", s.c_str(), k_);
	}

	void operator()(int i) const {
		printf("%d", i);
	}

	void operator()(double d) const {
		printf("%g", d);
	}

private:
	int k_;
	int n_;
};

void PrintLine(const std::string &uuid,
					  const Expr &lhs,
					  const Expr &rhs)
{
	const char *uuid_s = uuid.c_str();
	if (lhs.which() == kExprIsString) {
		const std::string &id(boost::get<std::string>(lhs));
		const char *id_s = id.c_str();
		printf("%s %s#2 ", uuid_s, id_s);
		boost::apply_visitor(VariantPrinter(2, 2), rhs);
		putchar('\n');
		printf("%s %s#4 ", uuid_s, id_s);
		boost::apply_visitor(VariantPrinter(4, 2), rhs);
		putchar('\n');
		printf("%s %s#6 ", uuid_s, id_s);
		boost::apply_visitor(VariantPrinter(6, 1), rhs);
		putchar('\n');
		printf("%s %s#0 ", uuid_s, id_s);
		boost::apply_visitor(VariantPrinter(0, 1), rhs);
		putchar('\n');
	} else {
		assert(lhs.which() == kExprIsCompound);
		const Compound &c(boost::get<Compound>(lhs));
		assert(c.children.size() == 2);
		const Expr &e(c.children.at(1));
		assert(e.which() == kExprIsString);
		const std::string &id(boost::get<std::string>(e));
		const char *id_s = id.c_str();

		// #1: k1 = dt * f(t_n, y_n)
		printf("%s %s#1 (times @dt ",
			   uuid_s, id_s);
		boost::apply_visitor(Printer(), rhs);
		printf(")\n");

		// #2: y1 = y_n + k1/2
		printf("%s %s#2 (plus %s (divide %s#1 2))\n",
			   uuid_s, id_s, id_s, id_s);

		// #3: k2 = dt * f(t_n + dt/2, y1)
		printf("%s %s#3 (times @dt ",
			   uuid_s, id_s);
		boost::apply_visitor(VariantPrinter(2, 2), rhs);
		printf(")\n");

		// #4: y2 = y_n + k2/2
		printf("%s %s#4 (plus %s (divide %s#3 2))\n",
			   uuid_s, id_s, id_s, id_s);

		// #5: k3 = dt * f(t_n + dt/2, y2)
		printf("%s %s#5 (times @dt ",
			   uuid_s, id_s);
		boost::apply_visitor(VariantPrinter(4, 2), rhs);
		printf(")\n");

		// #6: y3 = y_n + k3
		printf("%s %s#6 (plus %s %s#5)\n",
			   uuid_s, id_s, id_s, id_s);

		// #7: k4 = dt * f(t_n + dt, y3)
		printf("%s %s#7 (times @dt ",
			   uuid_s, id_s);
		boost::apply_visitor(VariantPrinter(6, 1), rhs);
		printf(")\n");

		// #0: y_(n+1) = y_n + (k1 + 2*k2 + 2*k3 + k4)/6
		printf("%s %s#0 (plus %s (divide (plus %s#1 (plus (times 2 %s#3) (plus (times 2 %s#5) %s#7))) 6))\n",
			   uuid_s,
			   id_s,
			   id_s,
			   id_s,
			   id_s,
			   id_s,
			   id_s);
	}
}

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

void ReportInvalidLeafOfCaseSet(const std::string &uuid)
{
	cerr << "invalid formula found in <case-set>: " << uuid << endl;
}

bool TransformConditional(const Compound &c, Expr &lhs, Compound &rhs)
{
	rhs.keyword = "piecewise";
	rhs.children.clear();

	std::vector<Expr>::const_iterator bit = c.children.begin();
	std::vector<Expr>::const_iterator eit = c.children.end();
	for (std::vector<Expr>::const_iterator it=bit;it!=eit;++it) {
		assert(it->which() == kExprIsCompound);
		const Compound &cs = boost::get<Compound>(*it);
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

void ProcessLine(Line &line)
{
	Compound &statement(line.statement);
	if (IsConditional(statement)) {
		Expr lhs;
		Compound rhs;
		if (!TransformConditional(statement, lhs, rhs)) {
			ReportInvalidLeafOfCaseSet(line.uuid);
			std::exit(EXIT_FAILURE);
		}
		PrintLine(line.uuid, lhs, rhs);
	} else if (IsEquation(statement)) {
		PrintLine(line.uuid,
				  statement.children[0],
				  statement.children[1]);
	} else {
		assert(false);
	}
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
		diff_ = "diff";
		eq_ = "eq";
		delay_ = "\"$\"Delay";
		delta_time_ = "\"$\"DeltaTime";

		uuid36 = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}";
		real = "{FLOAT}";
		integer = "{SIGN}?{DIGIT}+";
		id = "\"%\"[a-zA-Z_][a-zA-Z_0-9:]*";
		keyword = "[$]?[a-zA-Z_][a-zA-Z_0-9]*";

		this->self = lex::token_def<>('\n') | '\r' | '(' | ')' | ' ';
		this->self += case_set_ | case_ | condition_ | diff_ | eq_ | delay_ | delta_time_;
		this->self += uuid36 | real | integer | id | keyword;
	}

	lex::token_def<std::string> case_set_, case_, condition_, eq_, diff_, delay_, delta_time_;
	lex::token_def<std::string> uuid36, id, keyword;
	lex::token_def<int> integer;
	lex::token_def<double> real;
};

void RewriteDelayParam(Compound &x, const Expr &expr)
{
	Compound c;
	c.keyword = "minus";
	c.children.push_back("%time");
	c.children.push_back(expr);
	x.children.push_back(c);
}

void RewriteDeltaTimeParam(Compound &x, const Expr &expr)
{
	x.children.push_back(expr);

	Compound c;
	c.keyword = "minus";
	c.children.push_back("%time");
	c.children.push_back("@dt");
	x.children.push_back(c);
}

template<typename TIterator>
struct Grammar : qi::grammar<TIterator> {

	template<typename TTokenDef>
	Grammar(TTokenDef const &td)
	: Grammar::base_type(start)
	{
		using boost::phoenix::at_c;
		using boost::phoenix::push_back;
		using boost::phoenix::val;
		using boost::spirit::qi::eol;

		start = *(line [&ProcessLine] >> eol);

		line = td.uuid36 >> ' ' >> statement;

		statement = equation | conditional;

		equation = '(' >> td.eq_ [at_c<0>(_val) = _1]
					   >> ' ' >> (lexp | td.id) [push_back(at_c<1>(_val), _1)]
					   >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
					   >> ')';

		lexp = '(' >> td.diff_ [at_c<0>(_val) = _1]
				   >> ' ' >> expr [push_back(at_c<1>(_val), _1)]
				   >> ' ' >> td.id [push_back(at_c<1>(_val), _1)]
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

		delta_time_expr = '(' >> td.delta_time_ [at_c<0>(_val) = val("$lookback")]
							  >> ' ' >> expr [bind(&RewriteDeltaTimeParam, _val, _1)]
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

	qi::rule<TIterator> start;
	qi::rule<TIterator, Line()> line;
	qi::rule<TIterator, Expr()> expr, rest;
	qi::rule<TIterator, Compound()> statement, equation, lexp, conditional, cexp, delay_expr, delta_time_expr, eq_expr, general_expr;
	qi::rule<TIterator, std::vector<Expr>()> cseq, seq0, seq1;
};

bool ParseInput(std::istream &is)
{
	typedef std::istreambuf_iterator<char> input_iterator_type;
	typedef multi_pass<input_iterator_type> base_iterator_type;
	typedef lex::lexertl::token<base_iterator_type> token_type;
	typedef lex::lexertl::lexer<token_type> lexer_type;
	typedef Lexer<lexer_type> RealLexer;
	typedef Grammar<RealLexer::iterator_type> RealGrammar;

	static const RealLexer tokens;
	static const RealGrammar grammar(tokens);

	is.unsetf(std::ios::skipws);
	input_iterator_type iit(is);
	base_iterator_type it = make_default_multi_pass(iit);
	base_iterator_type eit;
	bool r = lex::tokenize_and_parse(it, eit, tokens, grammar);
	if (!r || it != eit) {
		cerr << "failed to parse: " << *it << endl;
		return false;
	}
	return true;
}

} // namespace

int main(void)
{
	printf("8\n"); /* 8 layers */
	if (!ParseInput(std::cin)) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
