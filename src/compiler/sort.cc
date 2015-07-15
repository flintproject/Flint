/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "sort.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_unordered_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "db/driver.hh"
#include "db/query.h"
#include "db/statement-driver.hh"
#include "lexer.hh"

using std::cerr;
using std::endl;
using std::string;
using std::vector;

using namespace boost::spirit;

namespace compiler {
namespace sort {

struct Compound;

typedef boost::variant<boost::recursive_wrapper<Compound>,
					   std::string,
					   int,
					   flint::lexer::Real
					   > Expr;

struct Compound {
	std::vector<Expr> children;
};

struct Entry {
	std::string uuid;
	std::string name;
	Expr expr;
};

}
}

BOOST_FUSION_ADAPT_STRUCT(compiler::sort::Compound,
						  (std::vector<compiler::sort::Expr>, children))

BOOST_FUSION_ADAPT_STRUCT(compiler::sort::Entry,
						  (std::string, uuid)
						  (std::string, name)
						  (compiler::sort::Expr, expr))

namespace compiler {
namespace sort {
namespace {

class DependencyCollector : public boost::static_visitor<> {
public:
	DependencyCollector(const string &name,
						const std::unordered_map<string, size_t> &candidates,
						std::unordered_set<size_t> *dependencies)
		: name_(name)
		, candidates_(candidates)
		, dependencies_(dependencies)
	{}

	void operator()(const Compound &c) const {
		std::vector<Expr>::const_iterator it, end = c.children.end();
		for (it=c.children.begin();it!=end;++it) {
			boost::apply_visitor(*this, *it);
		}
	}

	void operator()(const std::string &s) const {
		if (s == name_)
			return;
		std::unordered_map<string, size_t>::const_iterator it = candidates_.find(s);
		if (it != candidates_.cend())
			dependencies_->insert(it->second);
	}

	void operator()(int /*i*/) const {
		// nothing to do
	}

	void operator()(const flint::lexer::Real &) const {
		// nothing to do
	}

private:
	const string &name_;
	const std::unordered_map<string, size_t> &candidates_;
	std::unordered_set<size_t> *dependencies_;
};

class Printer : public boost::static_visitor<> {
public:
	explicit Printer(std::ostream *os)
		: os_(os)
	{
	}

	void operator()(const Compound &c) const {
		std::vector<Expr>::const_iterator bit = c.children.begin();
		std::vector<Expr>::const_iterator eit = c.children.end();
		os_->put('(');
		for (std::vector<Expr>::const_iterator it=bit;it!=eit;++it) {
			if (it != bit) os_->put(' ');
			boost::apply_visitor(*this, *it);
		}
		os_->put(')');
	}

	void operator()(const std::string &s) const {
		*os_ << s;
	}

	void operator()(int i) const {
		*os_ << i;
	}

	void operator()(const flint::lexer::Real &r) const {
		*os_ << r.lexeme;
	}

private:
	std::ostream *os_;
};

template<typename TLexer>
struct Lexer : lex::lexer<TLexer> {

	Lexer() {
		this->self.add_pattern
			("DIGIT", "[0-9]")
			("SIGN", "[-+]")
			("EXPONENT", "[eE]{SIGN}?{DIGIT}+")
			("FLOAT", "{SIGN}?({DIGIT}*\".\"{DIGIT}+{EXPONENT}?|{DIGIT}+{EXPONENT})")
			;

		uuid36 = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}";
		real = "{FLOAT}";
		integer = "{SIGN}?{DIGIT}+";
		id = "[%@][a-zA-Z_][a-zA-Z_0-9:#]*";
		keyword = "[$]?[a-zA-Z_][a-zA-Z_0-9]*";

		this->self = lex::token_def<>('\n') | '\r' | '(' | ')' | ' ';
		this->self += uuid36 | real | integer | id | keyword;
	}

	lex::token_def<std::string> uuid36, id, keyword;
	lex::token_def<int> integer;
	lex::token_def<flint::lexer::Real> real;
};

template<typename TIterator>
struct Grammar : qi::grammar<TIterator, Expr()> {

	template<typename TTokenDef>
	Grammar(TTokenDef const &td)
	: Grammar::base_type(expr)
	{
		expr %= (compound | td.real | td.integer | td.id | td.keyword);

		compound %= '(' >> (expr % ' ') >> ')';
	}

	qi::rule<TIterator, Expr()> expr;
	qi::rule<TIterator, Compound()> compound;
};

class Line : boost::noncopyable {
public:
	Line(const std::string &name, const Expr &expr)
		: name_(name),
		  expr_(expr)
	{
	}

	const string &name() const {return name_;}

	size_t CollectDependencies(const std::unordered_map<string, size_t> &candidates, std::unordered_set<size_t> *dependencies) {
		boost::apply_visitor(DependencyCollector(name_, candidates, dependencies), expr_);
		return dependencies->size();
	}

	std::string GetMath() const {
		std::ostringstream oss;
		boost::apply_visitor(Printer(&oss), expr_);
		return oss.str();
	}

private:
	std::string name_;
	Expr expr_;
};

class LineVector : boost::noncopyable {
public:
	size_t GetSize() const {
		return lines_.size();
	}

	void Add(Line *line) {
		lines_.push_back(line);
	}

	bool CalculateLevels(int *levels) {
		size_t n = lines_.size();
		std::unordered_map<string, size_t> nm;
		for (size_t i=0;i<n;i++) {
			auto p = nm.insert(std::make_pair(lines_[i].name(), i));
			if (!p.second) {
				cerr << "more than one entries for " << p.first->first << endl;
				return false;
			}
			levels[i] = -1;
		}
		std::unique_ptr<std::unordered_set<size_t>[]> dependencies(new std::unordered_set<size_t>[n]);
		size_t total = 0;
		for (size_t i=0;i<n;i++) {
			Line &line = lines_[i];
			if (line.CollectDependencies(nm, dependencies.get()+i) == 0) {
				levels[i] = 0;
				total++;
			}
		}
		if (total == 0) {
			// we have nothing to do if there is no level 0
			return true;
		}
		int level = 1;
		while (total < n) {
			bool found = false;
			for (size_t i=0;i<n;i++) {
				if (levels[i] >= 0) continue;
				if (std::all_of(dependencies[i].begin(), dependencies[i].end(),
								[&levels, level](size_t k){return 0 <= levels[k] && levels[k] < level;})) {
					levels[i] = level;
					total++;
					found = true;
				}
			}
			if (!found) {
				cerr << "failed to calculate level:";
				for (size_t i=0;i<n;i++) {
					if (levels[i] < 0)
						cerr << " " << lines_[i].name();
				}
				cerr << endl;
				return false;
			}
			level++;
		}
		return true;
	}

	const Line &at(size_t m) const {
		return lines_.at(m);
	}

private:
	boost::ptr_vector<Line> lines_;
};

class IndexAndLevel {
public:
	IndexAndLevel(size_t index, int level) : index_(index), level_(level) {}

	size_t index() const {return index_;}

	bool operator<(const IndexAndLevel &other) const {
		return level_ < other.level_;
	}

private:
	size_t index_;
	int level_;
};

typedef boost::ptr_unordered_map<string, LineVector> UuidMap;

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

	explicit Parser(UuidMap *um)
		: tokens_()
		, grammar_(tokens_)
		, um_(um)
	{
	}

	int Parse(const char *uuid, const char *name, const char *math) {
		base_iterator_type it = math;
		base_iterator_type eit = math + std::strlen(math);
		Expr expr;
		bool r = lex::tokenize_and_parse(it, eit, tokens_, grammar_, expr);
		if (!r || it != eit) {
			cerr << "failed to parse: " << *it << endl;
			return 1;
		}
		(*um_)[uuid].Add(new Line(name, expr));
		return 0;
	}

private:
	RealLexer tokens_;
	RealGrammar grammar_;
	UuidMap *um_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 3);
	Parser *parser = static_cast<Parser *>(data);
	return parser->Parse(argv[0], argv[1], argv[2]);
}

class Inserter : db::StatementDriver {
public:
	explicit Inserter(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO sorts VALUES (?, ?, ?)")
	{
	}

	bool Insert(const char *uuid, const char *name, const char *math) {
		int e;
		e = sqlite3_bind_text(stmt(), 1, uuid, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 2, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind name: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 3, math, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind math: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			cerr << "failed to step: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

}

bool Sort(sqlite3 *db)
{
	UuidMap um;
	{
		Parser parser(&um);
		char *em;
		int e;
		e = sqlite3_exec(db, "SELECT * FROM asts", Process, &parser, &em);
		if (e != SQLITE_OK) {
			cerr << "failed to exec: " << e
				 << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
	}

	if (!BeginTransaction(db))
		return false;
	if (!CreateTable(db, "sorts", "(uuid TEXT, name TEXT, math TEXT)"))
		return false;

	Inserter inserter(db);
	for (UuidMap::iterator umit=um.begin();umit!=um.end();++umit) {
		size_t n = umit->second->GetSize();
		std::unique_ptr<int[]> arr(new int[n]);
		if (!umit->second->CalculateLevels(arr.get())) {
			return false;
		}
		vector<IndexAndLevel> v;
		for (size_t k=0;k<n;k++) {
			v.push_back(IndexAndLevel(k, arr[k]));
		}
		std::stable_sort(v.begin(), v.end());
		for (vector<IndexAndLevel>::const_iterator vit=v.begin();vit!=v.end();++vit) {
			size_t m = vit->index();
			const Line &line(umit->second->at(m));
			std::string math = line.GetMath();
			if (!inserter.Insert(umit->first.c_str(),
								 line.name().c_str(),
								 math.c_str()))
				return false;
		}
	}
	return CommitTransaction(db);
}

}
}
