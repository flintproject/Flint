/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_DATABASE_DRIVER_H_
#define FLINT_PHML_DATABASE_DRIVER_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {
namespace phml {

class Arc;
class Bridge;
class Edge;
class Element;
class EventCondition;
class ExtraImplementation;
class Implementation;
class Import;
class InitialValue;
class Instance;
class Module;
class Node;
class NumericalConfiguration;
class PQ;
class Port;
class Reference;
class TargetModule;
class TargetPq;
class Template;
class TimeDiscretization;
class Timeseries;
class Unit;

class DatabaseDriver {
public:
	DatabaseDriver(const DatabaseDriver &) = delete;
	DatabaseDriver &operator=(const DatabaseDriver &) = delete;

	explicit DatabaseDriver(sqlite3 *db);
	~DatabaseDriver();

	bool Initialize();

	bool SaveNumericalConfiguration(const NumericalConfiguration *nc);

	bool SaveTimeDiscretization(const TimeDiscretization *td, const Module *module = nullptr);

	bool SaveUnit(Unit *unit);

	bool SaveElement(const Unit *unit, const Element *element);

	bool SaveModule(Module *module);

	bool SavePq(const Module *module, PQ *pq);

	bool UpdatePq(const PQ *pq, bool independent);

	bool SaveInitialValue(const PQ *pq, const InitialValue *iv);

	bool SaveImplementation(const PQ *pq, const Implementation *impl);

	bool SaveEventCondition(const PQ *pq, const EventCondition &ec);

	bool SaveNode(const PQ *pq, const Node *node);

	bool SaveArc(const PQ *pq, const Arc *arc);

	bool SaveReference(const PQ *pq, const Reference *reference, const ExtraImplementation *extra);

	bool SaveExtraImplementation(const PQ *pq, const ExtraImplementation *extra);

	bool SaveEdge(const Edge *edge);

	bool SaveBridge(const PQ *pq, const Bridge *bridge);

	bool SaveImport(const Module *module, const Import *import,
					const boost::filesystem::path &given_path,
					const boost::filesystem::path &model_path);

	bool SaveInstance(Instance *instance);

	bool SaveTargetModule(const Instance *instance, TargetModule *tm);

	bool SaveTargetPq(const TargetModule *tm, const TargetPq *tpq);

	bool SaveTemplate(const Template *t);

	bool SavePort(const Module *module, const Port *port);

	bool SaveTimeseries(const Module *module, const Timeseries *ts,
						const boost::filesystem::path &given_path,
						const boost::filesystem::path &model_path);

	bool SaveTsipc(const Module &module, const Timeseries &ts);

private:
	enum UpdateStatementIndex {
		kPq,
		kNumOfUpdateStatements
	};

	sqlite3 *db_;
	sqlite3_stmt *nc_stmt_;
	sqlite3_stmt *td_stmt_;
	sqlite3_stmt *unit_stmt_;
	sqlite3_stmt *element_stmt_;
	sqlite3_stmt *module_stmt_;
	sqlite3_stmt *pq_stmt_;
	sqlite3_stmt *iv_stmt_;
	sqlite3_stmt *impl_stmt_;
	sqlite3_stmt *event_condition_stmt_;
	sqlite3_stmt *node_stmt_;
	sqlite3_stmt *arc_stmt_;
	sqlite3_stmt *refport_stmt_;
	sqlite3_stmt *refts_stmt_;
	sqlite3_stmt *extra_stmt_;
	sqlite3_stmt *edge_stmt_;
	sqlite3_stmt *bridge_stmt_;
	sqlite3_stmt *import_stmt_;
	sqlite3_stmt *instance_stmt_;
	sqlite3_stmt *tm_stmt_;
	sqlite3_stmt *tpq_stmt_;
	sqlite3_stmt *template_stmt_;
	sqlite3_stmt *port_stmt_;
	sqlite3_stmt *timeseries_stmt_;
	sqlite3_stmt *tsipc_stmt_;
	sqlite3_stmt *update_stmt_[kNumOfUpdateStatements];
};

}
}

#endif
