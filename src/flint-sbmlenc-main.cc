/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

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

class Loader : boost::noncopyable {
public:
	explicit Loader(const char *file) : ifs_(file, std::ios::in) {}

	~Loader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(OdeVector *ov, AssignmentVector *av, CompartmentVector *cv) {
		static const int kUuidSize = 36;
		static const int kLineSize = kUuidSize + 10240; // FIXME

		if (!ifs_.is_open()) {
			cerr << "failed to open input file" << endl;
			return false;
		}
		boost::scoped_array<char> line(new char[kLineSize]);
		while (ifs_.getline(line.get(), kLineSize)) {
			size_t len = strlen(line.get());
			if (len < 2) {
				cerr << "invalid line: " << line.get() << endl;
				return false;
			}
			switch (line[0]) {
			case 'o':
				ov->push_back(Ode::Parse(line.get()+2));
				break;
			case 'a':
				av->push_back(Assignment::Parse(line.get()+2));
				break;
			case 'c':
				cv->push_back(Compartment::Parse(line.get()+2));
				break;
			default:
				cerr << "invalid line: " << line.get() << endl;
				return false;
			}
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

class NameWriter : boost::noncopyable {
public:
	explicit NameWriter(const char *file) : ofs_(file, std::ios::out), i_(1) {}

	~NameWriter() {
		if (ofs_.is_open()) ofs_.close();
	}

	template<typename TType>
	void Write(const TType &x) {
		ofs_ << "00000000-0000-0000-0000-000000000000 "
			 << x.type()
			 << ' '
			 << i_++
			 << ' '
			 << x.name()
			 << endl;
	}

private:
	std::ofstream ofs_;
	int i_;
};

class ValueWriter : boost::noncopyable {
public:
	explicit ValueWriter(const char *file) : ofs_(file, std::ios::out) {}

	~ValueWriter() {
		if (ofs_.is_open()) ofs_.close();
	}

	template<typename TType>
	void Write(const TType &x) {
		ofs_ << "00000000-0000-0000-0000-000000000000 (eq %"
			 << x.name()
			 << ' '
			 << x.value()
			 << ')'
			 << endl;
	}

private:
	std::ofstream ofs_;
};

class FunctionWriter : boost::noncopyable {
public:
	explicit FunctionWriter(const char *file) : ofs_(file, std::ios::out) {}

	~FunctionWriter() {
		if (ofs_.is_open()) ofs_.close();
	}

	void Write(const Assignment &a) {
		ofs_ << "00000000-0000-0000-0000-000000000000 (eq %"
			 << a.name()
			 << ' '
			 << a.rhs()
			 << ')'
			 << endl;
	}

private:
	std::ofstream ofs_;
};

class OdeWriter : boost::noncopyable {
public:
	explicit OdeWriter(const char *file) : ofs_(file, std::ios::out) {}

	~OdeWriter() {
		if (ofs_.is_open()) ofs_.close();
	}

	void Write(const Ode &o) {
		ofs_ << "00000000-0000-0000-0000-000000000000 (eq (diff (bvar %time) %"
			 << o.name()
			 << ") "
			 << o.rhs()
			 << ')'
			 << endl;
	}

private:
	std::ofstream ofs_;
};

void Usage()
{
	cerr << "usage: flint-sbmlenc INPUT NAME VALUE FUNCTION ODE" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			return EXIT_SUCCESS;
		} else {
			return EXIT_FAILURE;
		}
	}
	if (argc != 6) {
		Usage();
		return EXIT_FAILURE;
	}

	boost::scoped_ptr<OdeVector> ov(new OdeVector);
	boost::scoped_ptr<AssignmentVector> av(new AssignmentVector);
	boost::scoped_ptr<CompartmentVector> cv(new CompartmentVector);
	{
		boost::scoped_ptr<Loader> loader(new Loader(argv[1]));
		if (!loader->Load(ov.get(), av.get(), cv.get())) {
			return EXIT_FAILURE;
		}
	}

	{
		boost::scoped_ptr<NameWriter> writer(new NameWriter(argv[2]));
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

	{
		boost::scoped_ptr<ValueWriter> writer(new ValueWriter(argv[3]));
		for (OdeVector::const_iterator it=ov->begin();it!=ov->end();++it) {
			writer->Write(*it);
		}
		for (CompartmentVector::const_iterator it=cv->begin();it!=cv->end();++it) {
			writer->Write(*it);
		}
	}

	{
		boost::scoped_ptr<FunctionWriter> writer(new FunctionWriter(argv[4]));
		for (AssignmentVector::const_iterator it=av->begin();it!=av->end();++it) {
			writer->Write(*it);
		}
	}

	{
		boost::scoped_ptr<OdeWriter> writer(new OdeWriter(argv[5]));
		for (OdeVector::const_iterator it=ov->begin();it!=ov->end();++it) {
			writer->Write(*it);
		}
	}

	return EXIT_SUCCESS;
}
