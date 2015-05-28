/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include "db/driver.h"
#include "db/name-inserter.h"
#include "db/query.h"
#include "db/statement-driver.h"
#include "sbml.hh"

using std::cerr;
using std::endl;
using std::string;
using std::strlen;
using std::strtod;

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

typedef boost::ptr_vector<Ode> OdeVector;
typedef boost::ptr_vector<Assignment> AssignmentVector;
typedef boost::ptr_vector<Compartment> CompartmentVector;

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
	ov->push_back(new Ode(argv[0], argv[1], argv[2]));
	return 0;
}

int HandleAssignment(void *data, int argc, char **argv, char **names)
{
	(void)names;
	AssignmentVector *av = (AssignmentVector *)data;
	assert(argc == 2);
	av->push_back(new Assignment(argv[0], argv[1]));
	return 0;
}

int HandleConstant(void *data, int argc, char **argv, char **names)
{
	(void)names;
	CompartmentVector *cv = (CompartmentVector *)data;
	assert(argc == 2);
	cv->push_back(new Compartment(argv[0], argv[1]));
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
		return InsertName("00000000-0000-0000-0000-000000000000",
						  x.type(),
						  i_++,
						  x.name().c_str());
	}

private:
	int i_;
};

class ValueWriter : db::StatementDriver {
public:
	explicit ValueWriter(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO input_values VALUES ('00000000-0000-0000-0000-000000000000', ?)")
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
		: db::StatementDriver(db, "INSERT INTO input_functions VALUES ('00000000-0000-0000-0000-000000000000', ?)")
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
		: db::StatementDriver(db, "INSERT INTO input_odes VALUES ('00000000-0000-0000-0000-000000000000', ?)")
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

void Usage()
{
	cerr << "usage: flint-sbml DB" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	static const int kNumOfArgs = 2;

	if (argc != kNumOfArgs) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	db::Driver driver(argv[1]);
	sqlite3 *db = driver.db();

	if (!flint::sbml::Parse(db))
		return EXIT_FAILURE;

	if (!CreateSingleton(db))
		return EXIT_FAILURE;
	if (!BeginTransaction(db))
		return EXIT_FAILURE;

	boost::scoped_ptr<OdeVector> ov(new OdeVector);
	boost::scoped_ptr<AssignmentVector> av(new AssignmentVector);
	boost::scoped_ptr<CompartmentVector> cv(new CompartmentVector);
	{
		boost::scoped_ptr<Loader> loader(new Loader(db));
		if (!loader->Load(ov.get(), av.get(), cv.get())) {
			return EXIT_FAILURE;
		}
	}

	{
		boost::scoped_ptr<NameWriter> writer(new NameWriter(db));
		for (OdeVector::const_iterator it=ov->begin();it!=ov->end();++it) {
			writer->Write(*it);
		}
		for (AssignmentVector::const_iterator it=av->begin();it!=av->end();++it) {
			writer->Write(*it);
		}
		for (CompartmentVector::const_iterator it=cv->begin();it!=cv->end();++it) {
			writer->Write(*it);
		}
	}

	if (!CreateTable(db, "input_values", "(space_id TEXT, math TEXT)"))
		return EXIT_FAILURE;
	if (!CreateTable(db, "input_functions", "(space_id TEXT, math TEXT)"))
		return EXIT_FAILURE;
	if (!CreateTable(db, "input_odes", "(space_id TEXT, math TEXT)"))
		return EXIT_FAILURE;

	{
		boost::scoped_ptr<ValueWriter> writer(new ValueWriter(db));
		for (OdeVector::const_iterator it=ov->begin();it!=ov->end();++it) {
			if (!writer->Write(*it)) return false;
		}
		for (CompartmentVector::const_iterator it=cv->begin();it!=cv->end();++it) {
			if (!writer->Write(*it)) return false;
		}
	}

	{
		boost::scoped_ptr<FunctionWriter> writer(new FunctionWriter(db));
		for (AssignmentVector::const_iterator it=av->begin();it!=av->end();++it) {
			if (!writer->Write(*it)) return false;
		}
	}

	{
		boost::scoped_ptr<OdeWriter> writer(new OdeWriter(db));
		for (OdeVector::const_iterator it=ov->begin();it!=ov->end();++it) {
			if (!writer->Write(*it)) return false;
		}
	}

	if (!CreateView(db, "input_ivs",
					"SELECT * FROM input_values UNION ALL SELECT * FROM input_functions"))
		return EXIT_FAILURE;
	if (!CreateView(db, "input_eqs",
					"SELECT * FROM input_functions UNION ALL SELECT * FROM input_odes"))
		return EXIT_FAILURE;

	return CommitTransaction(db) ? EXIT_SUCCESS : EXIT_FAILURE;
}
