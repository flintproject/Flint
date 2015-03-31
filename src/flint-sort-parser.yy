%{
/* Prologue */
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <set>
#include <vector>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include "cas/sexp.h"

using std::cerr;
using std::endl;
using std::set;
using std::string;
using std::vector;

extern FILE *yyin;
int yylex();
void yyerror(char const *);

namespace {

void CollectDependencies(const string &name,
						 const Sexp *sexp,
						 const set<string> &candidates,
						 set<string> *dependencies)
{
	if (sexp->IsInt() || sexp->IsDouble()) return;
	if (sexp->IsSymbol()) {
		string s(sexp->s());
		if (s != name && candidates.count(s) > 0) {
			dependencies->insert(s);
		}
		return;
	}
	assert(sexp->IsCons());
	Sexp *rest = sexp->GetCdr();
	while (rest) {
		CollectDependencies(name, rest->GetCar(), candidates, dependencies);
		rest = rest->GetCdr();
	}
}

class Line : boost::noncopyable {
public:
	Line(const char *name, Sexp *sexp)
		: name_(name),
		  sexp_(sexp)
	{
	}

	~Line() {
		delete sexp_;
	}

	const string &name() const {return name_;}

	size_t CollectDependencies(const std::set<string> &candidates, set<string> *dependencies) {
		::CollectDependencies(name_, sexp_, candidates, dependencies);
		return dependencies->size();
	}

	void Print() const {
		printf("%s ", name_.c_str());
		sexp_->Print();
		putchar('\n');
	}

private:
	string name_;
	Sexp *sexp_;
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

void AddLine(const char *uuid, const char *name, Sexp *sexp)
{
	UuidMap *um = GetUuidMap();
	(*um)[uuid].Add(new Line(name, sexp));
}

} // namespace

static int nol;

%}

/* Bison declarations */
%error-verbose /* obsolete directive standing for "%define parse.error verbose" in Bison 2.7 or later */

%union {
	char *uuid;
	char *id;
	char *keyword;
	int i;
	double d;
	Sexp *sexp;
}

%token NEWLINE

%token <uuid> UUID36
%token <i> INTEGER
%token <d> REAL
%token <id> ID
%token <keyword> KEYWORD

%type <sexp> sexp seq0 seq1

%%
/* Grammer Rules */

input: head
    | input line
    ;

head: INTEGER NEWLINE {nol = $1;}

line: UUID36 ID sexp NEWLINE {AddLine($1, $2, $3);free($1);free($2);}
    ;

sexp: REAL {$$ = new Sexp($1);}
    | INTEGER {$$ = new Sexp($1);}
    | ID {$$ = new Sexp($1);}
    | KEYWORD {$$ = new Sexp($1);}
    | '(' KEYWORD seq0 ')' {$$ = new Sexp(new Sexp($2), $3);}
    ;

seq0: /* empty */ {$$ = NULL;}
    | seq1
    ;

seq1: sexp seq0 {$$ = new Sexp($1, $2);}
    ;

%%
/* Epilogue */

void yyerror(char const *s)
{
	std::fprintf(stderr, "%s\n", s);
}

int main(void)
{
	int r = yyparse();
	if (r != 0) return r;

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
