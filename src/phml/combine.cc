/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "combine.h"

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>

#include <boost/algorithm/string.hpp>
#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/bridge-loader.h"
#include "db/driver.h"
#include "db/name_loader.h"

using std::atoi;
using std::cerr;
using std::cin;
using std::endl;
using std::ifstream;
using std::ofstream;
using std::ios;
using std::make_pair;
using std::map;
using std::string;
using std::sscanf;
using std::strlen;
using std::strcmp;

namespace {

typedef map<string, string> BridgeMap;

BridgeMap *GetBridgeMap()
{
	static boost::scoped_ptr<BridgeMap> bm(new BridgeMap);
	return bm.get();
}

class LineWriter : boost::noncopyable {
public:
	LineWriter() : uuid_(NULL), ofs_() {}

    ~LineWriter() {
		ofs_.close();
	}

	void set_uuid(const char *uuid) {uuid_ = uuid;}

	bool OpenFile(const char *file) {
		ofs_.open(file, ios::out|ios::app|ios::binary);
		return ofs_.is_open();
	}

protected:
	const char *uuid_;
	ofstream ofs_;
};

class NameWriter : public LineWriter {
public:
	NameWriter() : LineWriter(), pq_id_() {}

	void set_pq_id(int pq_id) {pq_id_ = pq_id;}

	void Write(char c, const string &name) {
		ofs_ << uuid_ << " " << c << " " << ++pq_id_ << " " << name << endl;
	}

private:
	int pq_id_;
};

NameWriter *GetNameWriter()
{
	static boost::scoped_ptr<NameWriter> writer(new NameWriter);
	return writer.get();
}

class ValueWriter : public LineWriter {
public:
	ValueWriter() : LineWriter() {}

	template<typename TNumber>
	void Write(const string &name, TNumber x) {
		ofs_ << uuid_ << " " << "(eq %" << name << " " << x << ")" << endl;
	}
};

ValueWriter *GetValueWriter()
{
	static boost::scoped_ptr<ValueWriter> writer(new ValueWriter);
	return writer.get();
}

class FunctionWriter : public LineWriter {
public:
	FunctionWriter() : LineWriter() {}

	void WriteFunction(const string &name, const string &sexp) {
		ofs_ << uuid_ << " (eq %" << name << ' ' << sexp << ')' << endl;;
	}

	void WriteSet(const string &name, const string &pq_name) {
		ofs_ << uuid_ << " (eq %" << name << " %" << pq_name << ")" << endl;
	}

	void WriteGet(const string &pq_name, const string &name) {
		ofs_ << uuid_ << " (eq %" << pq_name << " %sbml:" << name << ")" << endl;
	}
};

FunctionWriter *GetFunctionWriter()
{
	static boost::scoped_ptr<FunctionWriter> writer(new FunctionWriter);
	return writer.get();
}

class OdeWriter : public LineWriter {
public:
	OdeWriter() : LineWriter() {}

	void WriteOde(const string &name, const string &sexp) {
		ofs_ << uuid_ << ' ' << "(eq (diff (bvar %time) %" << name << ") "
			 << sexp << ')' << endl;
	}
};

OdeWriter *GetOdeWriter()
{
	static boost::scoped_ptr<OdeWriter> writer(new OdeWriter);
	return writer.get();
}

template<typename TNumber>
void AddOde(const string &name, TNumber x, const string &sexp)
{
	const BridgeMap *bm = GetBridgeMap();
	BridgeMap::const_iterator it = bm->find(name);
	if (it == bm->end()) {
		GetNameWriter()->Write('x', name);
		GetValueWriter()->Write(name, x);
		GetOdeWriter()->WriteOde(name, sexp);
	} else {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteSet(name, it->second);
	}
}

void AddAssignment(const string &name, const string &sexp)
{
	const BridgeMap *bm = GetBridgeMap();
	BridgeMap::const_iterator it = bm->find(name);
	if (it == bm->end()) {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteFunction(name, sexp);
	} else {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteSet(name, it->second);
	}
}

template<typename TNumber>
void AddConstant(const string &name, TNumber x)
{
	const BridgeMap *bm = GetBridgeMap();
	BridgeMap::const_iterator it = bm->find(name);
	if (it == bm->end()) {
		GetNameWriter()->Write('s', name);
		GetValueWriter()->Write(name, x);
	} else {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteSet(name, it->second);
	}
}

typedef map<int, string> PhysicalQuantityMap;

class PhysicalQuantityHandler : boost::noncopyable {
public:
	PhysicalQuantityHandler(boost::uuids::uuid uuid, PhysicalQuantityMap *pqm) : uuid_(uuid), pqm_(pqm), max_pq_id_() {}

	bool Handle(boost::uuids::uuid u, char /*type*/, int pq_id, const char *name, const char * /*unit*/, double /*capacity*/) {
		if (uuid_ != u) return true; // skip this entry
		pqm_->insert(make_pair(pq_id, string(name)));
		max_pq_id_ = std::max(pq_id, max_pq_id_);
		return true;
	}

	int max_pq_id() const {return max_pq_id_;}

private:
	boost::uuids::uuid uuid_;
	PhysicalQuantityMap *pqm_;
	int max_pq_id_;
};

class BridgeHandler : boost::noncopyable {
public:
	BridgeHandler(boost::uuids::uuid uuid, PhysicalQuantityMap *pqm)
		: uuid_(uuid),
		  pqm_(pqm)
	{}

	bool Handle(boost::uuids::uuid uuid, int pq_id, const char *direction, const char * /*sub_type*/, const char *connector)
	{
		if (uuid_ != uuid) return true;
		if (strcmp(direction, "get") == 0) {
			PhysicalQuantityMap::const_iterator it = pqm_->find(pq_id);
			if (it == pqm_->end()) {
				cerr << "failed to find physical-quantity: " << pq_id << endl;
				return false;
			}
			GetFunctionWriter()->WriteGet(it->second, connector);
		} else if (strcmp(direction, "set") == 0) {
			PhysicalQuantityMap::const_iterator it = pqm_->find(pq_id);
			if (it == pqm_->end()) {
				cerr << "failed to find physical-quantity: " << pq_id << endl;
				return false;
			}
			GetBridgeMap()->insert(make_pair("sbml:" + string(connector), it->second));
		} else {
			cerr << "invalid bridge: " << uuid << ":" << pq_id << endl;
			return false;
		}
		return true;
	}

private:
	boost::uuids::uuid uuid_;
	PhysicalQuantityMap *pqm_;
};

size_t ProcessName(const string &s)
{
	if (!boost::starts_with(s, "sbml:")) {
		cerr << "name without prefix sbml: " << s << endl;
		return 0;
	}
	size_t i = s.find(' ');
	if (i == string::npos) {
		cerr << "no end sentinel for name: " << s << endl;
		return 0;
	}
	return i;
}

size_t ProcessNumber(const string &s)
{
	const char *q = s.c_str();
	char *r;
	(void)std::strtod(q, &r);
	if (q == r) {
		cerr << "failed to read double value: " << s << endl;
		return 0;
	}
	return r - q;
}

bool ProcessA(const string &s)
{
	size_t r = ProcessName(s);
	if (r == 0) return false;
	AddAssignment(s.substr(0, r), s.substr(r+1));
	return true;
}

bool ProcessC(const string &s)
{
	size_t r0 = ProcessName(s);
	if (r0 == 0) return false;
	string t = s.substr(r0+1);
	size_t r1 = ProcessNumber(t);
	if (r1 == 0) return false;
	AddConstant(s.substr(0, r0), t.substr(0, r1));
	return true;
}

bool ProcessO(const string &s)
{
	size_t r0 = ProcessName(s);
	if (r0 == 0) return false;
	string t = s.substr(r0+1);
	size_t r1 = ProcessNumber(t);
	if (r1 == 0) return false;
	AddOde(s.substr(0, r0), t.substr(0, r1), t.substr(r1+1));
	return true;
}

bool ProcessInput(std::istream &is)
{
	std::string line;

	is.unsetf(std::ios::skipws);
	while (std::getline(is, line)) {
		if (line.empty()) {
			cerr << "found an empty line" << endl;
			return false;
		}
		if (boost::starts_with(line, "a ")) {
			if (!ProcessA(line.substr(2))) return false;
		} else if (boost::starts_with(line, "c ")) {
			if (!ProcessC(line.substr(2))) return false;
		} else if (boost::starts_with(line, "o ")) {
			if (!ProcessO(line.substr(2))) return false;
		} else {
			cerr << "invalid line: " << line << endl;
			return false;
		}
	}
	return true;
}

} // namespace

bool Combine(const char *uuid,
			 const char *db_file,
			 const char *name_file,
			 const char *value_file,
			 const char *function_file,
			 const char *ode_file)
{
	boost::uuids::string_generator gen;
	boost::uuids::uuid u = gen(uuid);

	boost::scoped_ptr<db::Driver> driver(new db::Driver(db_file));

	boost::scoped_ptr<PhysicalQuantityMap> pqm(new PhysicalQuantityMap);
	int max_pq_id = 0;
	{
		boost::scoped_ptr<db::NameLoader> loader(new db::NameLoader(driver->db()));
		boost::scoped_ptr<PhysicalQuantityHandler> handler(new PhysicalQuantityHandler(u, pqm.get()));
		if (!loader->Load(handler.get())) {
			return false;
		}
		max_pq_id = handler->max_pq_id();
	}

	GetNameWriter()->set_pq_id(max_pq_id);
	GetNameWriter()->set_uuid(uuid);
	GetNameWriter()->OpenFile(name_file);
	GetValueWriter()->set_uuid(uuid);
	GetValueWriter()->OpenFile(value_file);
	GetFunctionWriter()->set_uuid(uuid);
	GetFunctionWriter()->OpenFile(function_file);
	GetOdeWriter()->set_uuid(uuid);
	GetOdeWriter()->OpenFile(ode_file);

	boost::scoped_ptr<BridgeMap> sm(new BridgeMap);
	{
		boost::scoped_ptr<db::BridgeLoader> loader(new db::BridgeLoader(driver->db()));
		boost::scoped_ptr<BridgeHandler> handler(new BridgeHandler(u, pqm.get()));
		if (!loader->Load(handler.get())) {
			return false;
		}
	}

	return ProcessInput(std::cin);
}
