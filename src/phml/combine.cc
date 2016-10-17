/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "combine.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <memory>
#include <sstream>
#include <string>

#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/bridge-loader.h"
#include "db/driver.h"
#include "db/eq-inserter.h"
#include "db/query.h"
#include "db/variable-inserter.h"
#include "db/variable-loader.h"
#include "variable.h"

using std::map;
using std::strcmp;

namespace flint {
namespace {

typedef map<std::string, std::string> BridgeMap;

class LineWriter {
public:
	LineWriter(const boost::uuids::uuid &uuid, const char *table, sqlite3 *db)
		: uuid_(uuid)
		, inserter_(table, db)
	{}

	bool Insert(const char *math) {
		return inserter_.Insert(uuid_, math);
	}

private:
	boost::uuids::uuid uuid_;
	db::EqInserter inserter_;
};

class VariableWriter : public db::VariableInserter {
public:
	VariableWriter(int id, const boost::uuids::uuid &uuid, sqlite3 *db)
		: db::VariableInserter("private_variables", false, db)
		, id_(id)
		, uuid_(uuid)
	{
	}

	bool Write(char c, const char *name) {
		return Insert(uuid_, c, ++id_, name);
	}

private:
	int id_;
	boost::uuids::uuid uuid_;
};

class ValueWriter : LineWriter {
public:
	ValueWriter(const boost::uuids::uuid &uuid, sqlite3 *db)
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
	FunctionWriter(const boost::uuids::uuid &uuid, sqlite3 *db)
		: LineWriter(uuid, "combined_functions", db)
	{}

	bool WriteFunction(const char *name, const char *sexp) {
		std::ostringstream oss;
		oss << "(eq %" << name << sexp << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}

	bool WriteSet(const char *name, const std::string &pq_name) {
		std::ostringstream oss;
		oss << "(eq %" << name << " %" << pq_name << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}

	bool WriteGet(const std::string &pq_name, const char *name) {
		std::ostringstream oss;
		oss << "(eq %" << pq_name << " %sbml:" << name << ')';
		std::string math = oss.str();
		return Insert(math.c_str());
	}
};

class OdeWriter : LineWriter {
public:
	OdeWriter(const boost::uuids::uuid &uuid, sqlite3 *db)
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
	Writer(int pq_id, const boost::uuids::uuid &uuid, sqlite3 *db)
		: variable_writer_(pq_id, uuid, db)
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
			if (!variable_writer_.Write('x', name)) return false;
			if (!value_writer_.Write(name, x))
				return false;
			if (!ode_writer_.WriteOde(name, sexp))
				return false;
		} else {
			if (!variable_writer_.Write('v', name)) return false;
			if (!function_writer_.WriteSet(name, it->second))
				return false;
		}
		return true;
	}

	bool AddAssignment(const char *name, const char *sexp)
	{
		BridgeMap::const_iterator it = bm_.find(name);
		if (it == bm_.end()) {
			if (!variable_writer_.Write('v', name)) return false;
			if (!function_writer_.WriteFunction(name, sexp))
				return false;
		} else {
			if (!variable_writer_.Write('v', name)) return false;
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
			if (!variable_writer_.Write('s', name)) return false;
			if (!value_writer_.Write(name, x))
				return false;
		} else {
			if (!variable_writer_.Write('v', name)) return false;
			if (!function_writer_.WriteSet(name, it->second))
				return false;
		}
		return true;
	}

private:
	BridgeMap bm_;
	VariableWriter variable_writer_;
	ValueWriter value_writer_;
	FunctionWriter function_writer_;
	OdeWriter ode_writer_;
};

typedef map<int, std::string> PhysicalQuantityMap;

class PhysicalQuantityHandler {
public:
	PhysicalQuantityHandler(const PhysicalQuantityHandler &) = delete;
	PhysicalQuantityHandler &operator=(const PhysicalQuantityHandler &) = delete;

	PhysicalQuantityHandler(const boost::uuids::uuid &uuid, PhysicalQuantityMap *pqm)
		: uuid_(uuid), pqm_(pqm), max_pq_id_()
	{}

	bool Handle(const boost::uuids::uuid &u, std::unique_ptr<Variable> &&var) {
		if (uuid_ != u) return true; // skip this entry
		pqm_->emplace(var->id(), var->name());
		max_pq_id_ = std::max(var->id(), max_pq_id_);
		return true;
	}

	int max_pq_id() const {return max_pq_id_;}

private:
	boost::uuids::uuid uuid_;
	PhysicalQuantityMap *pqm_;
	int max_pq_id_;
};

class BridgeHandler {
public:
	BridgeHandler(const BridgeHandler &) = delete;
	BridgeHandler &operator=(const BridgeHandler &) = delete;

	BridgeHandler(const boost::uuids::uuid &uuid, PhysicalQuantityMap *pqm,
				  Writer *writer)
		: uuid_(uuid)
		, pqm_(pqm)
		, writer_(writer)
	{}

	bool Handle(const boost::uuids::uuid &uuid, int pq_id, const char *direction, const char * /*sub_type*/, const char *connector)
	{
		if (uuid_ != uuid) return true;
		if (strcmp(direction, "get") == 0) {
			PhysicalQuantityMap::const_iterator it = pqm_->find(pq_id);
			if (it == pqm_->end()) {
				std::cerr << "failed to find physical-quantity: " << pq_id << std::endl;
				return false;
			}
			writer_->function_writer().WriteGet(it->second, connector);
		} else if (strcmp(direction, "set") == 0) {
			PhysicalQuantityMap::const_iterator it = pqm_->find(pq_id);
			if (it == pqm_->end()) {
				std::cerr << "failed to find physical-quantity: " << pq_id << std::endl;
				return false;
			}
			writer_->bm().emplace("sbml:" + std::string(connector), it->second);
		} else {
			std::cerr << "invalid bridge: " << uuid << ":" << pq_id << std::endl;
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
			if (e != SQLITE_ABORT)
				std::cerr << "failed to select assignments: " << e << ": " << em << std::endl;
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(db(), "SELECT * FROM constants", ProcessConstant, writer, &em);
		if (e != SQLITE_OK) {
			if (e != SQLITE_ABORT)
				std::cerr << "failed to select constants: " << e << ": " << em << std::endl;
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(db(), "SELECT * FROM odes", ProcessOde, writer, &em);
		if (e != SQLITE_OK) {
			if (e != SQLITE_ABORT)
				std::cerr << "failed to select odes: " << e << ": " << em << std::endl;
			sqlite3_free(em);
			return false;
		}
		return true;
	}
};

} // namespace

bool Combine(const boost::uuids::uuid &uuid, sqlite3 *db)
{
	PhysicalQuantityMap pqm;
	int max_pq_id = 0;
	{
		db::VariableLoader loader(db);
		PhysicalQuantityHandler handler(uuid, &pqm);
		if (!loader.Load(&handler)) {
			return false;
		}
		max_pq_id = handler.max_pq_id();
	}

	Writer writer(max_pq_id, uuid, db);

	{
		db::BridgeLoader loader(db);
		BridgeHandler handler(uuid, &pqm, &writer);
		if (!loader.Load(&handler)) {
			return false;
		}
	}

	std::ostringstream oss;
	oss << uuid << ".db";
	std::string uuid_db = oss.str();
	Loader loader(uuid_db.c_str());
	return loader.Load(&writer);
}

}
