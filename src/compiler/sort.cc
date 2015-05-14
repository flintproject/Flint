/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "sort.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iterator>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "db/driver.h"
#include "db/query.h"

using std::cerr;
using std::endl;
using std::set;
using std::string;
using std::vector;

using namespace boost::spirit;

namespace compiler {
namespace sort {

struct Compound;

typedef boost::variant<boost::recursive_wrapper<Compound>, std::string, int, double> Expr;

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
						const set<string> &candidates,
						set<string> *dependencies)
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
		if (s != name_ && candidates_.count(s) > 0) {
			dependencies_->insert(s);
		}
	}

	void operator()(int /*i*/) const {
		// nothing to do
	}

	void operator()(double /*d*/) const {
		// nothing to do
	}

private:
	const string &name_;
	const set<string> &candidates_;
	set<string> *dependencies_;
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

	void operator()(double d) const {
		*os_ << d;
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
	lex::token_def<double> real;
};

int nol;

void SetNol(int i) {
	nol = i;
}

void AddEntry(const Entry &entry);

template<typename TIterator>
struct Grammar : qi::grammar<TIterator> {

	template<typename TTokenDef>
	Grammar(TTokenDef const &td)
	: Grammar::base_type(start)
	{
		using boost::spirit::qi::eol;

		start = td.integer [&SetNol] >> eol >> input;

		input = *(entry [&AddEntry] >> eol);

		entry %= td.uuid36 >> ' ' >> td.id >> ' ' >> expr;

		expr %= (compound | td.real | td.integer | td.id | td.keyword);

		compound %= '(' >> (expr % ' ') >> ')';
	}

	qi::rule<TIterator> start;
	qi::rule<TIterator> input;
	qi::rule<TIterator, Entry()> entry;
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

	size_t CollectDependencies(const std::set<string> &candidates, set<string> *dependencies) {
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
		set<string> names;
		for (size_t i=0;i<n;i++) {
			names.insert(lines_[i].name());
			levels[i] = -1;
		}
		set<string> solved;
		boost::scoped_array<set<string> > dependencies(new set<string>[n]);
		for (size_t i=0;i<n;i++) {
			Line &line = lines_[i];
			if (line.CollectDependencies(names, dependencies.get()+i) == 0) {
				solved.insert(line.name());
				levels[i] = 0;
			}
		}
		if (solved.empty()) {
			// we have nothing to do if there is no level 0
			return true;
		}
		int level = 1;
		bool found = false;
		while (!std::includes(solved.begin(), solved.end(),
							  names.begin(), names.end())) {
			found = false;
			for (size_t i=0;i<n;i++) {
				if (levels[i] < 0) {
					const Line &line = lines_[i];
					if (std::includes(solved.begin(), solved.end(),
									  dependencies[i].begin(), dependencies[i].end())) {
						solved.insert(line.name());
						levels[i] = level;
						found = true;
					}
				}
			}
			if (!found) {
				set<string> unsolved;
				std::set_difference(names.begin(), names.end(),
									solved.begin(), solved.end(),
									std::inserter(unsolved, unsolved.end()));
				cerr << "failed to calculate level:";
				for (set<string>::const_iterator it=unsolved.begin();it!=unsolved.end();++it) {
					cerr << " " << *it;
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

typedef boost::ptr_map<string, LineVector> UuidMap;

UuidMap *GetUuidMap()
{
	static boost::scoped_ptr<UuidMap> um(new UuidMap);
	return um.get();
}

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

void AddEntry(const Entry &entry)
{
	UuidMap *um = GetUuidMap();
	(*um)[entry.uuid].Add(new Line(entry.name, entry.expr));
}

bool InsertNol(sqlite3 *db)
{
	static const char kQuery[] = "INSERT INTO nol VALUES (?)";

	bool r = false;
	int e;
	sqlite3_stmt *stmt;
	e = sqlite3_prepare_v2(db, kQuery, -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << e << endl;
		return false;
	}
	e = sqlite3_bind_int(stmt, 1, nol);
	if (e != SQLITE_OK) {
		cerr << "failed to bind nol: " << e << endl;
		goto bail;
	}
	e = sqlite3_step(stmt);
	if (e != SQLITE_DONE) {
		cerr << "failed to step: " << e << endl;
		goto bail;
	}
	r = true;

 bail:
	sqlite3_finalize(stmt);
	return r;
}

class Inserter {
public:
	Inserter(sqlite3 *db)
		: stmt_(NULL)
	{
		int e;
		e = sqlite3_prepare_v2(db, "INSERT INTO sorts VALUES (?, ?, ?)", -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
	}

	~Inserter() {
		sqlite3_finalize(stmt_);
	}

	bool Insert(const char *uuid, const char *name, const char *math) {
		int e;
		e = sqlite3_bind_text(stmt_, 1, uuid, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt_, 2, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind name: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt_, 3, math, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind math: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt_);
		if (e != SQLITE_DONE) {
			cerr << "failed to step: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt_);
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

}

bool Sort(std::istream &is, sqlite3 *db)
{
	if (!ParseInput(is)) return false;

	if (!BeginTransaction(db))
		return false;
	if (!CreateTable(db, "nol", "(nol INTEGER)"))
		return false;
	if (!InsertNol(db))
		return false;
	if (!CreateTable(db, "sorts", "(uuid TEXT, name TEXT, math TEXT)"))
		return false;

	Inserter inserter(db);
	for (UuidMap::iterator umit=GetUuidMap()->begin();umit!=GetUuidMap()->end();++umit) {
		size_t n = umit->second->GetSize();
		boost::scoped_array<int> arr(new int[n]);
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
