/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "graph-math-rewriter.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/variant/recursive_variant.hpp>

using std::cerr;
using std::endl;

using namespace boost::spirit;

// AST

struct Sexp;
struct Gexp;

typedef boost::variant<boost::recursive_wrapper<Sexp>, Gexp, std::string> Math;

struct Sexp {
	std::vector<Math> children;
};

struct Gexp {
	std::string lhs;
	std::string rhs;
};

BOOST_FUSION_ADAPT_STRUCT(Sexp, (std::vector<Math>, children))
BOOST_FUSION_ADAPT_STRUCT(Gexp, (std::string, lhs) (std::string, rhs))

namespace {

struct Detector : public boost::static_visitor<bool>
{
	bool operator()(const Sexp &sexp) const
	{
		const std::vector<Math> &c(sexp.children);
		for (std::vector<Math>::const_iterator it=c.begin();it!=c.end();++it) {
			if (boost::apply_visitor(Detector(), *it)) return true;
		}
		return false;
	}

	bool operator()(const Gexp &) const {return true;}

	bool operator()(const std::string &) const {return false;}
};

class Writer : public boost::static_visitor<bool>
{
public:
	Writer(phml::GraphMathRewriter *rewriter,
					 sqlite3_int64 pq_rowid,
					 std::ostringstream *oss)
		: rewriter_(rewriter),
		  pq_rowid_(pq_rowid),
		  oss_(oss)
	{
	}

	bool operator()(const Sexp &sexp) const
	{
		oss_->put('(');
		const std::vector<Math> &c(sexp.children);
		for (std::vector<Math>::const_iterator it=c.begin();it!=c.end();++it) {
			if (it != c.begin()) oss_->put(' ');
			if (!boost::apply_visitor(*this, *it)) return false;
		}
		oss_->put(')');
		return true;
	}

	bool operator()(const Gexp &gexp) const {
		const char *s = gexp.rhs.c_str();
		int node_id;
		if (!rewriter_->FindNode(pq_rowid_, &s[1], &node_id)) return false;
		*oss_ << "(eq " << gexp.lhs << ' ' << node_id << ')';
		return true;
	}

	bool operator()(const std::string &s) const
	{
		*oss_ << s;
		return true;
	}

private:
	phml::GraphMathRewriter *rewriter_;
	sqlite3_int64 pq_rowid_;
	std::ostringstream *oss_;
};

template<typename TLexer>
struct GraphMathLexer : lex::lexer<TLexer> {

	GraphMathLexer() {
		constant = "[^()%$ ]+";
		id = "%[^()%$ ]+";
		dollar_is = "\\$is";
		keyword = "\\$[^()%$ ]+";
		whitespace = "[ ]+";

		this->self = lex::token_def<>('(') | ')' | constant | id | dollar_is | keyword;

		this->self("WS") = whitespace;
	}

	lex::token_def<std::string> constant, id, keyword;
	lex::token_def<> dollar_is, whitespace;
};

template<typename TIterator, typename TLexer>
struct GraphMathGrammar : qi::grammar<TIterator, Math(), qi::in_state_skipper<TLexer> > {

	template<typename TTokenDef>
	GraphMathGrammar(TTokenDef const &td)
	: GraphMathGrammar::base_type(start)
	{
		using boost::phoenix::at_c;

		start = (gexp | sexp | td.constant | td.id | td.keyword);

		gexp = '(' >> td.dollar_is
				   >> td.id [at_c<0>(_val) = _1]
				   >> td.id [at_c<1>(_val) = _1]
				   >> ')' ;

		sexp %= '(' >> +start >> ')';

		BOOST_SPIRIT_DEBUG_NODE(start);
		BOOST_SPIRIT_DEBUG_NODE(gexp);
		BOOST_SPIRIT_DEBUG_NODE(sexp);
	}

	qi::rule<TIterator, Math(), qi::in_state_skipper<TLexer> > start;
	qi::rule<TIterator, Gexp(), qi::in_state_skipper<TLexer> > gexp;
	qi::rule<TIterator, Sexp(), qi::in_state_skipper<TLexer> > sexp;
};

const char kQueryNode[] = \
	"SELECT n.node_id FROM nodes AS n"
	" LEFT JOIN pqs AS p ON n.pq_rowid = p.rowid"
	" LEFT JOIN modules AS m ON p.module_rowid = m.rowid"
	" WHERE n.name = ?"
	" AND EXISTS (SELECT * FROM pqs WHERE rowid = ? AND module_rowid = m.rowid)";

} // namespace

namespace phml {

GraphMathRewriter::GraphMathRewriter(sqlite3 *db,
									 const char *query_select,
									 const char *query_update)
	: query_select_(query_select),
	  query_update_(query_update),
	  stmt_select_(NULL),
	  stmt_node_(NULL),
	  stmt_update_(NULL)
{
	int e = sqlite3_prepare_v2(db, query_select_, -1, &stmt_select_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << query_select_ << ": " << e << endl;
		exit(EXIT_FAILURE);
	}
	e = sqlite3_prepare_v2(db, kQueryNode, -1, &stmt_node_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << kQueryNode << ": " << e << endl;
		exit(EXIT_FAILURE);
	}
	e = sqlite3_prepare_v2(db, query_update_, -1, &stmt_update_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << query_update_ << ": " << e << endl;
		exit(EXIT_FAILURE);
	}
}

GraphMathRewriter::~GraphMathRewriter()
{
	sqlite3_finalize(stmt_select_);
	sqlite3_finalize(stmt_node_);
	sqlite3_finalize(stmt_update_);
}

bool GraphMathRewriter::Rewrite()
{
	int e;
	for (e = sqlite3_step(stmt_select_); e == SQLITE_ROW; e = sqlite3_step(stmt_select_)) {
		sqlite3_int64 rowid = sqlite3_column_int64(stmt_select_, 0);
		sqlite3_int64 pq_rowid = sqlite3_column_int64(stmt_select_, 1);
		const unsigned char *math = sqlite3_column_text(stmt_select_, 2);
		if (!Process(rowid, pq_rowid, (const char *)math)) return false;
	}
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << query_select_ << ": " << e << endl;
		return false;
	}
	return true;
}

bool GraphMathRewriter::Process(sqlite3_int64 rowid,
								sqlite3_int64 pq_rowid,
								const char *math)
{
	typedef const char * base_iterator_type;
	typedef lex::lexertl::token<base_iterator_type> token_type;
	typedef lex::lexertl::lexer<token_type> lexer_type;
	typedef GraphMathLexer<lexer_type> GML;
	typedef GML::iterator_type iterator_type;
	typedef GraphMathGrammar<iterator_type, GML::lexer_def> GMG;

	GML tokens;
	GMG grammar(tokens);
	const char *p = math;
	iterator_type it = tokens.begin(p, math + std::strlen(math));
	iterator_type end = tokens.end();
	Math ast;
	bool r = qi::phrase_parse(it, end, grammar, qi::in_state("WS")[tokens.self], ast);
	if (!r || it != end) {
		cerr << "failed to parse math: " << math << endl;
		return false;
	}
	if (!boost::apply_visitor(Detector(), ast)) return true;
	std::ostringstream oss;
	oss.put(' '); // a leading space
	if (!boost::apply_visitor(Writer(this, pq_rowid, &oss), ast)) return false;
	return Update(rowid, oss.str().c_str());
}

bool GraphMathRewriter::FindNode(sqlite3_int64 pq_rowid, const char *node_name, int *node_id)
{
	int e;
	e = sqlite3_bind_text(stmt_node_, 1, node_name, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind name: " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_bind_int64(stmt_node_, 2, pq_rowid);
	if (e != SQLITE_OK) {
		cerr << "failed to bind pq_rowid: " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt_node_);
	if (e != SQLITE_ROW) {
		cerr << "failed to find node with pq_rowid/named: "
			 << pq_rowid << '/' << node_name
			 << ": " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	int r = sqlite3_column_int(stmt_node_, 0);
	assert(r > 0);
	*node_id = r;
	sqlite3_reset(stmt_node_);
	return true;
}

bool GraphMathRewriter::Update(sqlite3_int64 rowid, const char *math)
{
	int e = sqlite3_bind_text(stmt_update_, 1, math, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind math: " << query_update_
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_bind_int64(stmt_update_, 2, rowid);
	if (e != SQLITE_OK) {
		cerr << "failed to bind rowid: " << query_update_
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt_update_);
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << query_update_
			 << ": " << e << endl;
		return false;
	}
	sqlite3_reset(stmt_update_);
	return true;
}

} // namespace phml
