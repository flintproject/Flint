/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "combine.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <map>

#include <boost/noncopyable.hpp>
#include <boost/scoped_array.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/bridge-loader.h"
#include "db/driver.h"
#include "db/name_loader.h"

using std::cerr;
using std::endl;
using std::ofstream;
using std::ios;
using std::make_pair;
using std::map;
using std::string;
using std::strlen;
using std::strcmp;

namespace {

typedef map<string, string> BridgeMap;

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

class ValueWriter : public LineWriter {
public:
	ValueWriter() : LineWriter() {}

	template<typename TNumber>
	void Write(const string &name, TNumber x) {
		ofs_ << uuid_ << " " << "(eq %" << name << " " << x << ")" << endl;
	}
};

class FunctionWriter : public LineWriter {
public:
	FunctionWriter() : LineWriter() {}

	void WriteFunction(const string &name, const string &sexp) {
		ofs_ << uuid_ << " (eq %" << name << sexp << ')' << endl;
	}

	void WriteSet(const string &name, const string &pq_name) {
		ofs_ << uuid_ << " (eq %" << name << " %" << pq_name << ")" << endl;
	}

	void WriteGet(const string &pq_name, const string &name) {
		ofs_ << uuid_ << " (eq %" << pq_name << " %sbml:" << name << ")" << endl;
	}
};

class OdeWriter : public LineWriter {
public:
	OdeWriter() : LineWriter() {}

	void WriteOde(const string &name, const string &sexp) {
		ofs_ << uuid_ << ' ' << "(eq (diff (bvar %time) %" << name << ')'
			 << sexp << ')' << endl;
	}
};

class Writer {
public:
	Writer(int pq_id, const char *uuid,
		   const char *name_file,
		   const char *value_file,
		   const char *function_file,
		   const char *ode_file)
	{
		name_writer_.set_pq_id(pq_id);
		name_writer_.set_uuid(uuid);
		name_writer_.OpenFile(name_file);
		value_writer_.set_uuid(uuid);
		value_writer_.OpenFile(value_file);
		function_writer_.set_uuid(uuid);
		function_writer_.OpenFile(function_file);
		ode_writer_.set_uuid(uuid);
		ode_writer_.OpenFile(ode_file);
	}

	BridgeMap &bm() {return bm_;}
	FunctionWriter &function_writer() {return function_writer_;}

	template<typename TNumber>
	void AddOde(const string &name, TNumber x, const string &sexp)
	{
		BridgeMap::const_iterator it = bm_.find(name);
		if (it == bm_.end()) {
			name_writer_.Write('x', name);
			value_writer_.Write(name, x);
			ode_writer_.WriteOde(name, sexp);
		} else {
			name_writer_.Write('v', name);
			function_writer_.WriteSet(name, it->second);
		}
	}

	void AddAssignment(const string &name, const string &sexp)
	{
		BridgeMap::const_iterator it = bm_.find(name);
		if (it == bm_.end()) {
			name_writer_.Write('v', name);
			function_writer_.WriteFunction(name, sexp);
		} else {
			name_writer_.Write('v', name);
			function_writer_.WriteSet(name, it->second);
		}
	}

	template<typename TNumber>
	void AddConstant(const string &name, TNumber x)
	{
		BridgeMap::const_iterator it = bm_.find(name);
		if (it == bm_.end()) {
			name_writer_.Write('s', name);
			value_writer_.Write(name, x);
		} else {
			name_writer_.Write('v', name);
			function_writer_.WriteSet(name, it->second);
		}
	}

private:
	BridgeMap bm_;
	NameWriter name_writer_;
	ValueWriter value_writer_;
	FunctionWriter function_writer_;
	OdeWriter ode_writer_;
};

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
	BridgeHandler(boost::uuids::uuid uuid, PhysicalQuantityMap *pqm,
				  Writer *writer)
		: uuid_(uuid)
		, pqm_(pqm)
		, writer_(writer)
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
			writer_->function_writer().WriteGet(it->second, connector);
		} else if (strcmp(direction, "set") == 0) {
			PhysicalQuantityMap::const_iterator it = pqm_->find(pq_id);
			if (it == pqm_->end()) {
				cerr << "failed to find physical-quantity: " << pq_id << endl;
				return false;
			}
			writer_->bm().insert(make_pair("sbml:" + string(connector), it->second));
		} else {
			cerr << "invalid bridge: " << uuid << ":" << pq_id << endl;
			return false;
		}
		return true;
	}

private:
	boost::uuids::uuid uuid_;
	PhysicalQuantityMap *pqm_;
	Writer *writer_;
};

int ProcessAssignment(void *data, int argc, char **argv, char **names)
{
	(void)names;
	Writer *writer = (Writer *)data;
	assert(argc == 2);
	writer->AddAssignment(argv[0], argv[1]);
	return 0;
}

int ProcessConstant(void *data, int argc, char **argv, char **names)
{
	(void)names;
	Writer *writer = (Writer *)data;
	assert(argc == 2);
	writer->AddConstant(argv[0], argv[1]);
	return 0;
}

int ProcessOde(void *data, int argc, char **argv, char **names)
{
	(void)names;
	Writer *writer = (Writer *)data;
	assert(argc == 3);
	writer->AddOde(argv[0], argv[1], argv[2]);
	return 0;
}

class Loader : public db::Driver {
public:
	explicit Loader(const char *db_file)
		: db::Driver(db_file)
	{
	}

	bool Load(Writer *writer) {
		int e;
		char *em;
		e = sqlite3_exec(db(), "SELECT * FROM assignments", ProcessAssignment, writer, &em);
		if (e != SQLITE_OK) {
			cerr << e << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(db(), "SELECT * FROM constants", ProcessConstant, writer, &em);
		if (e != SQLITE_OK) {
			cerr << e << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(db(), "SELECT * FROM odes", ProcessOde, writer, &em);
		if (e != SQLITE_OK) {
			cerr << e << ": " << em << endl;
			sqlite3_free(em);
			return false;
		}
		return true;
	}
};

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

	db::Driver driver(db_file);

	PhysicalQuantityMap pqm;
	int max_pq_id = 0;
	{
		db::NameLoader loader(driver.db());
		PhysicalQuantityHandler handler(u, &pqm);
		if (!loader.Load(&handler)) {
			return false;
		}
		max_pq_id = handler.max_pq_id();
	}

	Writer writer(max_pq_id, uuid, name_file, value_file, function_file, ode_file);

	{
		db::BridgeLoader loader(driver.db());
		BridgeHandler handler(u, &pqm, &writer);
		if (!loader.Load(&handler)) {
			return false;
		}
	}

	boost::scoped_array<char> uuid_db(new char[strlen(uuid)+4]);
	std::sprintf(uuid_db.get(), "%s.db", uuid);
	Loader loader(uuid_db.get());
	return loader.Load(&writer);
}
