/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "sort.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/driver.h"
#include "db/query.h"
#include "db/statement-driver.h"
#include "flint/sexp.h"
#include "flint/sexp/parser.h"

using std::cerr;
using std::endl;
using std::memcpy;
using std::string;

namespace flint {
namespace compiler {
namespace sort {
namespace {

class DependencyCollector : public sexp::Visitor<void> {
public:
	DependencyCollector(const string &name,
						const std::unordered_map<string, size_t> &candidates,
						std::unordered_set<size_t> *dependencies)
		: name_(name)
		, candidates_(candidates)
		, dependencies_(dependencies)
	{}

	void operator()(const sexp::Compound &c) {
		for (auto &child : c.children())
			sexp::ApplyVisitor(*this, *child);
	}

	void operator()(const sexp::Identifier &x) {
		const auto &t = x.token();
		if (t.type == sexp::Token::Type::kKeyword)
			return;
		assert(t.type == sexp::Token::Type::kIdentifier);
		auto s = x.GetString();
		if (s == name_)
			return;
		std::unordered_map<string, size_t>::const_iterator it = candidates_.find(s);
		if (it != candidates_.cend())
			dependencies_->insert(it->second);
	}

	void operator()(const sexp::Literal &/*a*/) {
		// nothing to do
	}

private:
	const string &name_;
	const std::unordered_map<string, size_t> &candidates_;
	std::unordered_set<size_t> *dependencies_;
};

class Line {
public:
	Line(const Line &) = delete;
	Line &operator=(const Line &) = delete;

	Line(const char *name,
		 std::unique_ptr<char[]> &&math,
		 std::unique_ptr<sexp::Expression> &&expr)
		: name_(name)
		, math_(std::move(math))
		, expr_(std::move(expr))
	{
	}

	const string &name() const {return name_;}

	size_t CollectDependencies(const std::unordered_map<string, size_t> &candidates, std::unordered_set<size_t> *dependencies) {
		DependencyCollector dc(name_, candidates, dependencies);
		sexp::ApplyVisitor(dc, *expr_);
		return dependencies->size();
	}

	std::string GetMath() const {
		return std::string(math_.get());
	}

private:
	std::string name_;
	std::unique_ptr<char[]> math_;
	std::unique_ptr<sexp::Expression> expr_;
};

class LineVector {
public:
	LineVector(const LineVector &) = delete;
	LineVector &operator=(const LineVector &) = delete;

	LineVector() {}

	size_t GetSize() const {
		return lines_.size();
	}

	void Add(const char *name,
			 std::unique_ptr<char[]> &&math,
			 std::unique_ptr<sexp::Expression> &&expr) {
		lines_.emplace_back(new Line(name, std::move(math), std::move(expr)));
	}

	bool CalculateLevels(const boost::uuids::uuid &uuid, int *levels) {
		size_t n = lines_.size();
		std::unordered_map<string, size_t> nm;
		for (size_t i=0;i<n;i++) {
			auto p = nm.insert(std::make_pair(lines_[i]->name(), i));
			if (!p.second) {
				cerr << "more than one entries for " << p.first->first
					 << " in " << uuid
					 << endl;
				return false;
			}
			levels[i] = -1;
		}
		std::unique_ptr<std::unordered_set<size_t>[]> dependencies(new std::unordered_set<size_t>[n]);
		size_t total = 0;
		for (size_t i=0;i<n;i++) {
			auto &line = lines_[i];
			if (line->CollectDependencies(nm, dependencies.get()+i) == 0) {
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
				cerr << "failed to calculate level in "
					 << uuid
					 << ':'
					 << endl;
				for (size_t i=0;i<n;i++) {
					if (levels[i] < 0)
						cerr << ' ' << lines_[i]->name()
							 << ": "
							 << lines_[i]->GetMath()
							 << endl;
				}
				return false;
			}
			level++;
		}
		return true;
	}

	const Line &at(size_t m) const {
		return *lines_.at(m);
	}

private:
	std::vector<std::unique_ptr<Line> > lines_;
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

typedef std::unordered_map<boost::uuids::uuid,
						   LineVector,
						   boost::hash<boost::uuids::uuid> > UuidMap;

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 3);
	UuidMap *um = static_cast<UuidMap *>(data);
	assert(argv[0]);
	assert(argv[1]);
	assert(argv[2]);
	boost::uuids::uuid u;
	memcpy(&u, argv[0], u.size());
	std::unique_ptr<char[]> math(new char[std::strlen(argv[2])+1]);
	std::strcpy(math.get(), argv[2]);
	std::unique_ptr<sexp::Expression> expr;
	sexp::parser::Parser parser(math.get());
	if (parser(&expr) <= 0)
		return 1;
	(*um)[u].Add(argv[1], std::move(math), std::move(expr));
	return 0;
}

class Inserter : db::StatementDriver {
public:
	explicit Inserter(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO sorts VALUES (?, ?, ?)")
	{
	}

	bool Insert(const boost::uuids::uuid &uuid, const char *name, const char *math) {
		int e;
		e = sqlite3_bind_blob(stmt(), 1, &uuid, uuid.size(), SQLITE_STATIC);
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
		char *em;
		int e;
		e = sqlite3_exec(db, "SELECT * FROM asts", Process, &um, &em);
		if (e != SQLITE_OK) {
			if (e != SQLITE_ABORT)
				cerr << "failed to select asts: " << e << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
	}

	if (!BeginTransaction(db))
		return false;
	if (!CreateTable(db, "sorts", "(uuid BLOB, name TEXT, math TEXT)"))
		return false;

	Inserter inserter(db);
	for (UuidMap::iterator umit=um.begin();umit!=um.end();++umit) {
		size_t n = umit->second.GetSize();
		std::unique_ptr<int[]> arr(new int[n]);
		if (!umit->second.CalculateLevels(umit->first, arr.get())) {
			return false;
		}
		std::vector<IndexAndLevel> v;
		for (size_t k=0;k<n;k++) {
			v.push_back(IndexAndLevel(k, arr[k]));
		}
		std::stable_sort(v.begin(), v.end());
		for (const auto &ial : v) {
			size_t m = ial.index();
			const Line &line(umit->second.at(m));
			std::string math = line.GetMath();
			if (!inserter.Insert(umit->first,
								 line.name().c_str(),
								 math.c_str()))
				return false;
		}
	}
	return CommitTransaction(db);
}

}
}
}
