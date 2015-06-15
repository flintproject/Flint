/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "combine.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <sstream>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/scoped_array.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/bridge-loader.h"
#include "db/driver.h"
#include "db/eq-inserter.h"
#include "db/name-inserter.h"
#include "db/query.h"
#include "db/name_loader.h"

using std::cerr;
using std::endl;
using std::make_pair;
using std::map;
using std::string;
using std::strlen;
using std::strcmp;

namespace {

typedef map<string, string> BridgeMap;

class LineWriter {
public:
	LineWriter(const char *uuid, const char *table, sqlite3 *db)
		: uuid_(uuid)
		, inserter_(table, db)
	{}

	bool Insert(const char *math) {
		return inserter_.Insert(uuid_, math);
	}

private:
	const char *uuid_;
	db::EqInserter inserter_;
};

class NameWriter : public db::NameInserter {
public:
	NameWriter(int id, const char *uuid, sqlite3 *db)
		: db::NameInserter("private_names", db)
		, id_(id)
		, uuid_(uuid)
	{
	}

	bool Write(char c, const char *name) {
		return InsertName(uuid_, c, ++id_, name);
	}

private:
	int id_;
	const char *uuid_;
};

class ValueWriter : LineWriter {
public:
	ValueWriter(const char *uuid, sqlite3 *db)
		: LineWriter(uuid, "combined_values", db)
	{}

	template<typename TNumber>
	bool Write(const char *name, TNumber x) {
		std::ostringstream oss;
		oss << "(eq %" << name << ' ' << x << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}
};

class FunctionWriter : LineWriter {
public:
	FunctionWriter(const char *uuid, sqlite3 *db)
		: LineWriter(uuid, "combined_functions", db)
	{}

	bool WriteFunction(const char *name, const char *sexp) {
		std::ostringstream oss;
		oss << "(eq %" << name << sexp << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}

	bool WriteSet(const char *name, const string &pq_name) {
		std::ostringstream oss;
		oss << "(eq %" << name << " %" << pq_name << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}

	bool WriteGet(const string &pq_name, const char *name) {
		std::ostringstream oss;
		oss << "(eq %" << pq_name << " %sbml:" << name << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}
};

class OdeWriter : LineWriter {
public:
	OdeWriter(const char *uuid, sqlite3 *db)
		: LineWriter(uuid, "combined_odes", db)
	{}

	bool WriteOde(const char *name, const char *sexp) {
		std::ostringstream oss;
		oss << "(eq (diff (bvar %time) %" << name << ')'
			<< sexp << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}
};

class Writer {
public:
	Writer(int pq_id, const char *uuid, sqlite3 *db)
		: name_writer_(pq_id, uuid, db)
		, value_writer_(uuid, db)
		, function_writer_(uuid, db)
		, ode_writer_(uuid, db)
	{
	}

	BridgeMap &bm() {return bm_;}
	FunctionWriter &function_writer() {return function_writer_;}

	template<typename TNumber>
	bool AddOde(const char *name, TNumber x, const char *sexp)
	{
		BridgeMap::const_iterator it = bm_.find(name);
		if (it == bm_.end()) {
			if (!name_writer_.Write('x', name)) return false;
			if (!value_writer_.Write(name, x))
				return false;
			if (!ode_writer_.WriteOde(name, sexp))
				return false;
		} else {
			if (!name_writer_.Write('v', name)) return false;
			if (!function_writer_.WriteSet(name, it->second))
				return false;
		}
		return true;
	}

	bool AddAssignment(const char *name, const char *sexp)
	{
		BridgeMap::const_iterator it = bm_.find(name);
		if (it == bm_.end()) {
			if (!name_writer_.Write('v', name)) return false;
			if (!function_writer_.WriteFunction(name, sexp))
				return false;
		} else {
			if (!name_writer_.Write('v', name)) return false;
			if (!function_writer_.WriteSet(name, it->second))
				return false;
		}
		return true;
	}

	template<typename TNumber>
	bool AddConstant(const char *name, TNumber x)
	{
		BridgeMap::const_iterator it = bm_.find(name);
		if (it == bm_.end()) {
			if (!name_writer_.Write('s', name)) return false;
			if (!value_writer_.Write(name, x))
				return false;
		} else {
			if (!name_writer_.Write('v', name)) return false;
			if (!function_writer_.WriteSet(name, it->second))
				return false;
		}
		return true;
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
	Writer *writer = static_cast<Writer *>(data);
	assert(argc == 2);
	return (writer->AddAssignment(argv[0], argv[1])) ? 0 : 1;
}

int ProcessConstant(void *data, int argc, char **argv, char **names)
{
	(void)names;
	Writer *writer = static_cast<Writer *>(data);
	assert(argc == 2);
	return (writer->AddConstant(argv[0], argv[1])) ? 0 : 1;
}

int ProcessOde(void *data, int argc, char **argv, char **names)
{
	(void)names;
	Writer *writer = static_cast<Writer *>(data);
	assert(argc == 3);
	return (writer->AddOde(argv[0], argv[1], argv[2])) ? 0 : 1;
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

bool Combine(const char *uuid, sqlite3 *db)
{
	boost::uuids::string_generator gen;
	boost::uuids::uuid u = gen(uuid);

	PhysicalQuantityMap pqm;
	int max_pq_id = 0;
	{
		db::NameLoader loader(db);
		PhysicalQuantityHandler handler(u, &pqm);
		if (!loader.Load(&handler)) {
			return false;
		}
		max_pq_id = handler.max_pq_id();
	}

	Writer writer(max_pq_id, uuid, db);

	{
		db::BridgeLoader loader(db);
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
