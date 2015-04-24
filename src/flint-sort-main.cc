/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <set>
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

using std::cerr;
using std::endl;
using std::set;
using std::string;
using std::vector;

using namespace boost::spirit;

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

BOOST_FUSION_ADAPT_STRUCT(Compound,
						  (std::vector<Expr>, children))

BOOST_FUSION_ADAPT_STRUCT(Entry,
						  (std::string, uuid)
						  (std::string, name)
						  (Expr, expr))

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
	void operator()(const Compound &c) const {
		std::vector<Expr>::const_iterator bit = c.children.begin();
		std::vector<Expr>::const_iterator eit = c.children.end();
		putchar('(');
		for (std::vector<Expr>::const_iterator it=bit;it!=eit;++it) {
			if (it != bit) putchar(' ');
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
		whitespace = ' ';

		this->self = lex::token_def<>('\n') | '\r' | '(' | ')' | uuid36 | real | integer | id | keyword;

		this->self("WS") = whitespace;
	}

	lex::token_def<std::string> uuid36, id, keyword;
	lex::token_def<int> integer;
	lex::token_def<double> real;
	lex::token_def<> whitespace;
};

static int nol;

static void SetNol(int i) {
	nol = i;
}

static void AddEntry(const Entry &entry);

template<typename TIterator, typename TLexer>
struct Grammar : qi::grammar<TIterator, qi::in_state_skipper<TLexer> > {

	template<typename TTokenDef>
	Grammar(TTokenDef const &td)
	: Grammar::base_type(start)
	{
		using boost::spirit::qi::eol;

		start = td.integer [&SetNol] >> eol >> input;

		input = *(entry [&AddEntry] >> eol);

		entry %= td.uuid36 >> td.id >> expr;

		expr %= (compound | td.real | td.integer | td.id | td.keyword);

		compound %= '(' >> +expr >> ')';
	}

	qi::rule<TIterator, qi::in_state_skipper<TLexer> > start;
	qi::rule<TIterator, qi::in_state_skipper<TLexer> > input;
	qi::rule<TIterator, Entry(), qi::in_state_skipper<TLexer> > entry;
	qi::rule<TIterator, Expr(), qi::in_state_skipper<TLexer> > expr;
	qi::rule<TIterator, Compound(), qi::in_state_skipper<TLexer> > compound;
};

namespace {

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

	void Print() const {
		printf("%s ", name_.c_str());
		boost::apply_visitor(Printer(), expr_);
		putchar('\n');
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

	void Print(size_t m) const {
		lines_.at(m).Print();
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
	typedef Grammar<RealLexer::iterator_type, RealLexer::lexer_def> RealGrammar;

	static const RealLexer tokens;
	static const RealGrammar grammar(tokens);

	is.unsetf(std::ios::skipws);
	input_iterator_type iit(is);
	base_iterator_type it = make_default_multi_pass(iit);
	base_iterator_type eit;
	bool r = lex::tokenize_and_phrase_parse(it, eit,
											tokens, grammar,
											qi::in_state("WS")[tokens.self]);
	if (!r || it != eit) {
		cerr << "failed to parse: " << *it << endl;
		return false;
	}
	return true;
}

} // namespace

static void AddEntry(const Entry &entry)
{
	UuidMap *um = GetUuidMap();
	(*um)[entry.uuid].Add(new Line(entry.name, entry.expr));
}

int main(void)
{
	if (!ParseInput(std::cin)) return EXIT_FAILURE;

	printf("%d\n", nol);
	for (UuidMap::iterator umit=GetUuidMap()->begin();umit!=GetUuidMap()->end();++umit) {
			size_t n = umit->second->GetSize();
			boost::scoped_array<int> arr(new int[n]);
			if (!umit->second->CalculateLevels(arr.get())) {
				return EXIT_FAILURE;
			}
			vector<IndexAndLevel> v;
			for (size_t k=0;k<n;k++) {
				v.push_back(IndexAndLevel(k, arr[k]));
			}
			std::stable_sort(v.begin(), v.end());
			for (vector<IndexAndLevel>::const_iterator vit=v.begin();vit!=v.end();++vit) {
				size_t m = vit->index();
				printf("%s ", umit->first.c_str());
				umit->second->Print(m);
			}
	}
	return EXIT_SUCCESS;
}
