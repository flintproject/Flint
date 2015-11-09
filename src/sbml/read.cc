/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "sbml.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "db/name-inserter.h"
#include "db/query.h"
#include "db/statement-driver.hh"
#include "sbml.hh"

using std::cerr;
using std::endl;
using std::string;
using std::strtod;

namespace flint {
namespace {

class Ode {
public:
	Ode(const char *name, const char *value, const char *rhs)
		: name_(name),
		  value_(strtod(value, NULL)),
		  rhs_(rhs)
	{}

	static Ode *Parse(char *rest) {
		size_t s = 1;
		while (rest[s] != ' ') s++;
		rest[s++] = '\0';
		const char *value = rest + s;
		while (rest[s] != ' ') s++;
		rest[s++] = '\0';
		return new Ode(rest, value, rest+s);
	}

	char type() const {return 'x';}
	const string &name() const {return name_;}
	double value() const {return value_;}
	const string &rhs() const {return rhs_;}

private:
	string name_;
	double value_;
	string rhs_;
};

class Assignment {
public:
	Assignment(const char *name, const char *rhs) : name_(name), rhs_(rhs) {}

	static Assignment *Parse(char *rest) {
		size_t s = 1;
		while (rest[s] != ' ') s++;
		rest[s++] = '\0';
		return new Assignment(rest, rest+s);
	}

	char type() const {return 'v';}
	const string &name() const {return name_;}
	const string &rhs() const {return rhs_;}

private:
	string name_;
	string rhs_;
};

class Compartment {
public:
	Compartment(const char *name, const char *value) : name_(name), value_(strtod(value, NULL)) {}

	static Compartment *Parse(char *rest) {
		size_t s = 1;
		while (rest[s] != ' ') s++;
		rest[s++] = '\0';
		return new Compartment(rest, rest+s);
	}

	char type() const {return 's';}
	const string &name() const {return name_;}
	double value() const {return value_;}

private:
	string name_;
	double value_;
};

typedef std::vector<Ode> OdeVector;
typedef std::vector<Assignment> AssignmentVector;
typedef std::vector<Compartment> CompartmentVector;

int HandleOde(void *data, int argc, char **argv, char **names);
int HandleAssignment(void *data, int argc, char **argv, char **names);
int HandleConstant(void *data, int argc, char **argv, char **names);

class Loader {
public:
	explicit Loader(sqlite3 *db)
		: db_(db)
	{
	}

	bool Load(OdeVector *ov, AssignmentVector *av, CompartmentVector *cv) {
		int e;
		char *em;
		e = sqlite3_exec(db_, "SELECT * FROM odes", HandleOde, ov, &em);
		if (e != SQLITE_OK) {
			cerr << e << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(db_, "SELECT * FROM assignments", HandleAssignment, av, &em);
		if (e != SQLITE_OK) {
			cerr << e << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(db_, "SELECT * FROM constants", HandleConstant, cv, &em);
		if (e != SQLITE_OK) {
			cerr << e << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
		return true;
	}

private:
	sqlite3 *db_;
};

int HandleOde(void *data, int argc, char **argv, char **names)
{
	(void)names;
	OdeVector *ov = (OdeVector *)data;
	assert(argc == 3);
	ov->emplace_back(argv[0], argv[1], argv[2]);
	return 0;
}

int HandleAssignment(void *data, int argc, char **argv, char **names)
{
	(void)names;
	AssignmentVector *av = (AssignmentVector *)data;
	assert(argc == 2);
	av->emplace_back(argv[0], argv[1]);
	return 0;
}

int HandleConstant(void *data, int argc, char **argv, char **names)
{
	(void)names;
	CompartmentVector *cv = (CompartmentVector *)data;
	assert(argc == 2);
	cv->emplace_back(argv[0], argv[1]);
	return 0;
}

class NameWriter : public db::NameInserter {
public:
	explicit NameWriter(sqlite3 *db)
		: db::NameInserter("names", db)
		, i_(1)
	{
	}

	template<typename TType>
	bool Write(const TType &x) {
		return InsertName(x.type(),
						  i_++,
						  x.name().c_str());
	}

private:
	int i_;
};

class ValueWriter : db::StatementDriver {
public:
	explicit ValueWriter(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO input_values VALUES (X'00000000000000000000000000000000', ?)")
	{
	}

	template<typename TType>
	bool Write(const TType &x) {
		std::ostringstream oss;
		oss << "(eq %" << x.name() << ' ' << x.value() << ')';
		std::string math = oss.str();
		int e;
		e = sqlite3_bind_text(stmt(), 1, math.c_str(), -1, SQLITE_STATIC);
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

class FunctionWriter : db::StatementDriver {
public:
	explicit FunctionWriter(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO input_functions VALUES (X'00000000000000000000000000000000', ?)")
	{}

	bool Write(const Assignment &a) {
		std::ostringstream oss;
		oss << "(eq %" << a.name() << a.rhs() << ')';
		std::string math = oss.str();
		int e;
		e = sqlite3_bind_text(stmt(), 1, math.c_str(), -1, SQLITE_STATIC);
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

class OdeWriter : db::StatementDriver {
public:
	explicit OdeWriter(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO input_odes VALUES (X'00000000000000000000000000000000', ?)")
	{}

	bool Write(const Ode &o) {
		std::ostringstream oss;
		oss << "(eq (diff (bvar %time) %"
			<< o.name()
			<< ')'
			<< o.rhs()
			<< ')';
		std::string math = oss.str();
		int e;
		e = sqlite3_bind_text(stmt(), 1, math.c_str(), -1, SQLITE_STATIC);
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

namespace sbml {

bool Read(sqlite3 *db)
{
	if (!flint::sbml::Parse(db))
		return false;

	if (!CreateSingleton(db))
		return false;
	if (!BeginTransaction(db))
		return false;

	std::unique_ptr<OdeVector> ov(new OdeVector);
	std::unique_ptr<AssignmentVector> av(new AssignmentVector);
	std::unique_ptr<CompartmentVector> cv(new CompartmentVector);
	{
		std::unique_ptr<Loader> loader(new Loader(db));
		if (!loader->Load(ov.get(), av.get(), cv.get())) {
			return false;
		}
	}

	{
		std::unique_ptr<NameWriter> writer(new NameWriter(db));
		for (const auto &ode : *ov) {
			writer->Write(ode);
		}
		for (const auto &a : *av) {
			writer->Write(a);
		}
		for (const auto &c : *cv) {
			writer->Write(c);
		}
	}

	if (!CreateTable(db, "input_values", "(space_id BLOB, math TEXT)"))
		return false;
	if (!CreateTable(db, "input_functions", "(space_id BLOB, math TEXT)"))
		return false;
	if (!CreateTable(db, "input_odes", "(space_id BLOB, math TEXT)"))
		return false;

	{
		std::unique_ptr<ValueWriter> writer(new ValueWriter(db));
		for (const auto &ode : *ov) {
			if (!writer->Write(ode)) return false;
		}
		for (const auto &c : *cv) {
			if (!writer->Write(c)) return false;
		}
	}

	{
		std::unique_ptr<FunctionWriter> writer(new FunctionWriter(db));
		for (const auto &a : *av) {
			if (!writer->Write(a)) return false;
		}
	}

	{
		std::unique_ptr<OdeWriter> writer(new OdeWriter(db));
		for (const auto &ode : *ov) {
			if (!writer->Write(ode)) return false;
		}
	}

	if (!CreateView(db, "input_ivs",
					"SELECT * FROM input_values UNION ALL SELECT * FROM input_functions"))
		return false;
	if (!CreateView(db, "input_eqs",
					"SELECT * FROM input_functions UNION ALL SELECT * FROM input_odes"))
		return false;
	if (!CreateLayout(db))
		return false;

	return CommitTransaction(db);
}

}
}
