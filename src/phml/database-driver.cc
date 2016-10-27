/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml/database-driver.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>

#include <boost/uuid/string_generator.hpp>

#include "utf8path.h"
#include "phml/arc.h"
#include "phml/bridge.h"
#include "phml/definition.h"
#include "phml/edge.h"
#include "phml/element.h"
#include "phml/event-condition.h"
#include "phml/extra-implementation.h"
#include "phml/implementation.h"
#include "phml/import.h"
#include "phml/initial-value.h"
#include "phml/instance.h"
#include "phml/module.h"
#include "phml/node.h"
#include "phml/numerical-configuration.h"
#include "phml/port.h"
#include "phml/pq.h"
#include "phml/reference.h"
#include "phml/target-module.h"
#include "phml/target-pq.h"
#include "phml/template.h"
#include "phml/time-discretization.h"
#include "phml/timeseries.h"
#include "phml/unit.h"
#include "reduction.h"

namespace flint {
namespace phml {

namespace {

template<typename TElement>
bool GetAbsolutePathFromReference(const TElement *element,
								  const boost::filesystem::path &given_path,
								  const boost::filesystem::path &model_path,
								  boost::filesystem::path *output_path)
{
	if (element->iref()) {
		boost::filesystem::path iref_path = GetPathFromUtf8(reinterpret_cast<const char *>(element->iref()));
		if (iref_path.empty())
			return false;
		if (iref_path.is_absolute()) {
			*output_path = iref_path;
			return true;
		}
		boost::filesystem::path gpp(given_path.parent_path());
		*output_path = boost::filesystem::absolute(iref_path, gpp);
		return true;
	}
	if (element->zref()) {
		boost::filesystem::path zref_path = GetPathFromUtf8(reinterpret_cast<const char *>(element->zref()));
		if (zref_path.empty())
			return false;
		if (zref_path.is_absolute()) {
			std::cerr << "found an absolute zref: "
				 << element->zref()
				 << std::endl;
			return false;
		}
		boost::filesystem::path mpp(model_path.parent_path());
		*output_path = boost::filesystem::absolute(zref_path, mpp);
		return true;
	}
	return false;
}

}

DatabaseDriver::DatabaseDriver(sqlite3 *db)
	: db_(db)
	, nc_stmt_(nullptr)
	, td_stmt_(nullptr)
	, unit_stmt_(nullptr)
	, element_stmt_(nullptr)
	, module_stmt_(nullptr)
	, pq_stmt_(nullptr)
	, iv_stmt_(nullptr)
	, impl_stmt_(nullptr)
	, event_condition_stmt_(nullptr)
	, node_stmt_(nullptr)
	, arc_stmt_(nullptr)
	, refport_stmt_(nullptr)
	, refts_stmt_(nullptr)
	, extra_stmt_(nullptr)
	, edge_stmt_(nullptr)
	, bridge_stmt_(nullptr)
	, import_stmt_(nullptr)
	, instance_stmt_(nullptr)
	, tm_stmt_(nullptr)
	, tpq_stmt_(nullptr)
	, template_stmt_(nullptr)
	, port_stmt_(nullptr)
	, timeseries_stmt_(nullptr)
{
	for (int i=0;i<kNumOfUpdateStatements;i++) {
		update_stmt_[i] = nullptr;
	}
}

DatabaseDriver::~DatabaseDriver()
{
	sqlite3_finalize(nc_stmt_);
	sqlite3_finalize(td_stmt_);
	sqlite3_finalize(unit_stmt_);
	sqlite3_finalize(element_stmt_);
	sqlite3_finalize(module_stmt_);
	sqlite3_finalize(pq_stmt_);
	sqlite3_finalize(iv_stmt_);
	sqlite3_finalize(impl_stmt_);
	sqlite3_finalize(event_condition_stmt_);
	sqlite3_finalize(node_stmt_);
	sqlite3_finalize(arc_stmt_);
	sqlite3_finalize(refport_stmt_);
	sqlite3_finalize(refts_stmt_);
	sqlite3_finalize(extra_stmt_);
	sqlite3_finalize(edge_stmt_);
	sqlite3_finalize(bridge_stmt_);
	sqlite3_finalize(import_stmt_);
	sqlite3_finalize(instance_stmt_);
	sqlite3_finalize(tm_stmt_);
	sqlite3_finalize(tpq_stmt_);
	sqlite3_finalize(template_stmt_);
	sqlite3_finalize(port_stmt_);
	sqlite3_finalize(timeseries_stmt_);
	sqlite3_finalize(update_stmt_[kPq]);
}

bool DatabaseDriver::Initialize()
{
	int e;

	// prepare statements
	e = sqlite3_prepare_v2(db_, "INSERT INTO ncs VALUES (?, ?, ?, ?, ?)",
						   -1, &nc_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO tds VALUES (?, ?, ?)",
						   -1, &td_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO units VALUES (?, ?)",
						   -1, &unit_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO elements VALUES (?, ?, ?, ?, ?, ?)",
						   -1, &element_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO modules VALUES (?, ?, ?, ?, ?)",
						   -1, &module_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO pqs (module_rowid, type, pq_id) VALUES (?, ?, ?)",
						   -1, &pq_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO ivs VALUES (?, ?)",
						   -1, &iv_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO impls VALUES (?, ?)",
						   -1, &impl_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO ecs VALUES (?, ?)",
						   -1, &event_condition_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO nodes VALUES (?, ?, ?)",
						   -1, &node_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO arcs VALUES (?, ?, ?, ?, ?)",
						   -1, &arc_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO refports VALUES (?, ?, ?)",
						   -1, &refport_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO refts VALUES (?, ?, ?)",
						   -1, &refts_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO extras VALUES (?, ?, ?)",
						   -1, &extra_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO edges VALUES (?, ?, ?, ?)",
						   -1, &edge_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO bridges VALUES (?, ?, ?, ?)",
						   -1, &bridge_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO imports VALUES (?, ?, ?)",
						   -1, &import_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO instances VALUES (?, ?, ?)",
						   -1, &instance_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO tms VALUES (?, ?)",
						   -1, &tm_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO tpqs VALUES (?, ?, ?)",
						   -1, &tpq_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO templates VALUES (?, ?)",
						   -1, &template_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO ports VALUES (?, ?, ?, ?, ?)",
						   -1, &port_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "INSERT INTO timeseries VALUES (?, ?, ?, ?)",
						   -1, &timeseries_stmt_, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	e = sqlite3_prepare_v2(db_, "UPDATE pqs SET unit_id = ?, name = ?, ncols = ?, nrows = ?, max_delay = ?, independent = ? WHERE rowid = ?",
						   -1, &update_stmt_[kPq], nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}
	return true;
}

bool DatabaseDriver::SaveNumericalConfiguration(const NumericalConfiguration *nc)
{
	int e;
	e = sqlite3_bind_text(nc_stmt_, 1, reinterpret_cast<const char *>(nc->rg_name()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind rg_name: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(nc_stmt_, 2, reinterpret_cast<const char *>(nc->rg_seed()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind rg_seed: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(nc_stmt_, 3, reinterpret_cast<const char *>(nc->integration()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind integration: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(nc_stmt_, 4, nc->sts_unit_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind sts_unit_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(nc_stmt_, 5, reinterpret_cast<const char *>(nc->sts_value()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind sts_value: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(nc_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(nc_stmt_);
	return true;
}

bool DatabaseDriver::SaveTimeDiscretization(const TimeDiscretization *td, const Module *module)
{
	int e;
	e = sqlite3_bind_int(td_stmt_, 1, td->unit_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind unit_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(td_stmt_, 2, reinterpret_cast<const char *>(td->step()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind step: " << e << std::endl;
		return false;
	}
	boost::uuids::uuid u;
	if (module) {
		u = module->GetUuid();
		e = sqlite3_bind_blob(td_stmt_, 3, &u, u.size(), SQLITE_STATIC);
	} else {
		e = sqlite3_bind_null(td_stmt_, 3);
	}
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(td_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(td_stmt_);
	return true;
}

bool DatabaseDriver::SaveUnit(Unit *unit)
{
	int e;
	e = sqlite3_bind_int(unit_stmt_, 1, unit->unit_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind unit_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(unit_stmt_, 2, reinterpret_cast<const char *>(unit->name()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind name: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(unit_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(unit_stmt_);

	sqlite3_int64 rowid = sqlite3_last_insert_rowid(db_);
	unit->set_rowid(rowid);

	return true;
}

bool DatabaseDriver::SaveElement(const Unit *unit, const Element *element)
{
	enum {
		kUnitRowid = 1,
		kUnitId,
		kExponent,
		kFactor,
		kMultiplier,
		kOffset
	};

	int e;
	e = sqlite3_bind_int64(element_stmt_, kUnitRowid, unit->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind unit_rowid: " << e << std::endl;
		return false;
	}

	e = sqlite3_bind_int(element_stmt_, kUnitId, element->unit_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind unit_id: " << e << std::endl;
		return false;
	}

	if (element->exponent() == 0) {
		e = sqlite3_bind_null(element_stmt_, kExponent);
	} else {
		e = sqlite3_bind_double(element_stmt_, kExponent, element->exponent());
	}
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind exponent: " << e << std::endl;
		return false;
	}

	if (element->factor() == 0) {
		e = sqlite3_bind_null(element_stmt_, kFactor);
	} else {
		e = sqlite3_bind_int(element_stmt_, kFactor, element->factor());
	}
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind factor: " << e << std::endl;
		return false;
	}

	if (element->multiplier() == 0) {
		e = sqlite3_bind_null(element_stmt_, kMultiplier);
	} else {
		e = sqlite3_bind_double(element_stmt_, kMultiplier, element->multiplier());
	}
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind multiplier: " << e << std::endl;
		return false;
	}

	if (element->offset() == 0) {
		e = sqlite3_bind_null(element_stmt_, kOffset);
	} else {
		e = sqlite3_bind_double(element_stmt_, kOffset, element->offset());
	}
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind offset: " << e << std::endl;
		return false;
	}

	e = sqlite3_step(element_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(element_stmt_);
	return true;
}

bool DatabaseDriver::SaveModule(Module *module)
{
	if (!module->IsValid()) {
		std::cerr << "module is invalid" << std::endl;
		return false;
	}

	int e;
	boost::uuids::uuid u = module->GetUuid();
	e = sqlite3_bind_blob(module_stmt_, 1, &u, u.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(module_stmt_, 2, reinterpret_cast<const char *>(module->type()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind type: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(module_stmt_, 3, reinterpret_cast<const char *>(module->name()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind name: " << e << std::endl;
		return false;
	}
	boost::uuids::uuid cu;
	if (module->capsulated_by()) {
		cu = module->GetCapsulatedBy();
		e = sqlite3_bind_blob(module_stmt_, 4, &cu, cu.size(), SQLITE_STATIC);
	} else {
		e = sqlite3_bind_null(module_stmt_, 4);
	}
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind capsulated_by: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(module_stmt_, 5, reinterpret_cast<const char *>(module->template_state()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind template_state: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(module_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(module_stmt_);

	sqlite3_int64 rowid = sqlite3_last_insert_rowid(db_);
	module->set_rowid(rowid);

	return true;
}

bool DatabaseDriver::SavePq(const Module *module, PQ *pq)
{
	if (!module->IsSaved()) {
		std::cerr << "module is not saved yet: "
			 << module->module_id()
			 << std::endl;
		return false;
	}
	if (!pq->IsValid()) {
		std::cerr << "pq is invalid" << std::endl;
		return false;
	}

	int e;
	e = sqlite3_bind_int64(pq_stmt_, 1, module->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(pq_stmt_, 2, reinterpret_cast<const char *>(pq->GetType()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind type: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(pq_stmt_, 3, pq->pq_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(pq_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(pq_stmt_);

	sqlite3_int64 rowid = sqlite3_last_insert_rowid(db_);
	pq->set_rowid(rowid);

	return true;
}

bool DatabaseDriver::UpdatePq(const PQ *pq, bool independent)
{
	assert(pq->IsSaved());
	sqlite3_stmt *stmt = update_stmt_[kPq];
	int e;
	e = sqlite3_bind_text(stmt, 1, reinterpret_cast<const char *>(pq->unit_id()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind unit_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(stmt, 2, reinterpret_cast<const char *>(pq->name()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind name: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(stmt, 3, pq->col());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind ncols: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(stmt, 4, pq->row());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind nrows: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(stmt, 5, reinterpret_cast<const char *>(pq->max_delay()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind max_delay: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(stmt, 6, independent ? 1 : 0);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind independent: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int64(stmt, 7, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(stmt);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(stmt);
	return true;
}

bool DatabaseDriver::SaveInitialValue(const PQ *pq, const InitialValue *iv)
{
	int e;
	e = sqlite3_bind_int64(iv_stmt_, 1, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_rowid: " << e << std::endl;
		return false;
	}
	std::string math = iv->GetString();
	e = sqlite3_bind_text(iv_stmt_, 2, math.c_str(), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind math: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(iv_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(iv_stmt_);
	return true;
}

bool DatabaseDriver::SaveImplementation(const PQ *pq, const Implementation *impl)
{
	int e;
	e = sqlite3_bind_int64(impl_stmt_, 1, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_rowid: " << e << std::endl;
		return false;
	}
	std::string math = impl->GetString();
	e = sqlite3_bind_text(impl_stmt_, 2, math.c_str(), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind math: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(impl_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(impl_stmt_);
	return true;
}

bool DatabaseDriver::SaveEventCondition(const PQ *pq, const EventCondition &ec)
{
	int e;
	e = sqlite3_bind_int64(event_condition_stmt_, 1, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_rowid: " << e << std::endl;
		return false;
	}
	std::string math = ec.GetMath();
	e = sqlite3_bind_text(event_condition_stmt_, 2, math.c_str(), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind math: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(event_condition_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(event_condition_stmt_);
	return true;
}

bool DatabaseDriver::SaveNode(const PQ *pq, const Node *node)
{
	int e;
	e = sqlite3_bind_int64(node_stmt_, 1, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(node_stmt_, 2, node->node_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind node_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(node_stmt_, 3, reinterpret_cast<const char *>(node->name()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind name: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(node_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(node_stmt_);
	return true;
}

bool DatabaseDriver::SaveArc(const PQ *pq, const Arc *arc)
{
	int e;
	e = sqlite3_bind_int64(arc_stmt_, 1, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(arc_stmt_, 2, arc->tail_node_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind tail_node_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(arc_stmt_, 3, arc->head_node_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind head_node_id: " << e << std::endl;
		return false;
	}
	assert(arc->type() != Arc::kUnspecified);
	e = sqlite3_bind_text(arc_stmt_, 4, (arc->type() == Arc::kCondition) ? "condition" : "probability", -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind type: " << e << std::endl;
		return false;
	}
	std::string math = arc->GetMath();
	e = sqlite3_bind_text(arc_stmt_, 5, math.c_str(), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind math: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(arc_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(arc_stmt_);
	return true;
}

bool DatabaseDriver::SaveReference(const PQ *pq, const Reference *reference, const ExtraImplementation *extra)
{
	int e;
	if (reference->port_id() > 0) { // reference to port
		e = sqlite3_bind_int64(refport_stmt_, 1, pq->rowid());
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind pq_rowid: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int(refport_stmt_, 2, reference->port_id());
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind port_id: " << e << std::endl;
			return false;
		}
		if ( extra &&
			 extra->definition() &&
			 xmlStrEqual(extra->definition()->type(), reinterpret_cast<const xmlChar *>("reduction")) ) {
			Reduction reduction;
			if (!ConvertStringToReduction(reinterpret_cast<const char *>(extra->definition()->sub_type()),
										  &reduction))
				return false;
			e = sqlite3_bind_int(refport_stmt_, 3, static_cast<int>(reduction));
		} else {
			// assume "sum" by default, for backward compatibility
			e = sqlite3_bind_int(refport_stmt_, 3, static_cast<int>(Reduction::kSum));
		}
		e = sqlite3_step(refport_stmt_);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(refport_stmt_);
	} else { // reference to timeseries
		e = sqlite3_bind_int64(refts_stmt_, 1, pq->rowid());
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind pq_rowid: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int(refts_stmt_, 2, reference->timeseries_id());
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind timeseries_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_text(refts_stmt_, 3, reinterpret_cast<const char *>(reference->element_id()), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind element_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(refts_stmt_);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(refts_stmt_);
	}
	return true;
}

bool DatabaseDriver::SaveExtraImplementation(const PQ *pq, const ExtraImplementation *extra)
{
	int e;
	e = sqlite3_bind_int64(extra_stmt_, 1, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(extra_stmt_, 2, reinterpret_cast<const char *>(extra->order()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind order_type: " << e << std::endl;
		return false;
	}
	std::string math = extra->GetString();
	e = sqlite3_bind_text(extra_stmt_, 3, math.c_str(), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind math: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(extra_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(extra_stmt_);
	return true;
}

bool DatabaseDriver::SaveEdge(const Edge *edge)
{
	int e;
	boost::uuids::uuid tu = edge->GetUuidOfTailModuleId();
	boost::uuids::uuid hu = edge->GetUuidOfHeadModuleId();
	e = sqlite3_bind_blob(edge_stmt_, 1, &tu, tu.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind tail_module_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(edge_stmt_, 2, reinterpret_cast<const char *>(edge->tail_port_id()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind tail_port_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_blob(edge_stmt_, 3, &hu, hu.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind head_module_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(edge_stmt_, 4, reinterpret_cast<const char *>(edge->head_port_id()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind head_port_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(edge_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(edge_stmt_);
	return true;
}

bool DatabaseDriver::SaveBridge(const PQ *pq, const Bridge *bridge)
{
	int e;
	e = sqlite3_bind_int64(bridge_stmt_, 1, pq->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(bridge_stmt_, 2, reinterpret_cast<const char *>(bridge->direction()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind direction: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(bridge_stmt_, 3, reinterpret_cast<const char *>(bridge->sub_type()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind sub_type: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(bridge_stmt_, 4, reinterpret_cast<const char *>(bridge->connector()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind connector: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(bridge_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(bridge_stmt_);
	return true;
}

bool DatabaseDriver::SaveImport(const Module *module, const Import *import,
								const boost::filesystem::path &given_path,
								const boost::filesystem::path &model_path)
{
	if (!module->IsSaved()) {
		std::cerr << "module is not saved yet: "
			 << module->module_id()
			 << std::endl;
		return false;
	}

	int e;
	e = sqlite3_bind_int64(import_stmt_, 1, module->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(import_stmt_, 2, reinterpret_cast<const char *>(import->type()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind type: " << e << std::endl;
		return false;
	}

	std::unique_ptr<char[]> utf8;
	if (xmlStrEqual(import->type(), reinterpret_cast<const xmlChar *>("external"))) {
		boost::filesystem::path path;
		if (!GetAbsolutePathFromReference(import, given_path, model_path, &path)) {
			std::cerr << "external <import> without reference" << std::endl;
			return false;
		}
		utf8.reset(GetUtf8FromPath(path));
		if (!utf8)
			return false;
		e = sqlite3_bind_text(import_stmt_, 3, utf8.get(), -1, SQLITE_STATIC);
	} else {
		e = sqlite3_bind_null(import_stmt_, 3);
	}
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind ref: " << e << std::endl;
		return false;
	}

	e = sqlite3_step(import_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(import_stmt_);
	return true;
}

bool DatabaseDriver::SaveInstance(Instance *instance)
{
	int e;
	boost::uuids::uuid mu = instance->GetUuidOfModuleId();
	boost::uuids::uuid tu = instance->GetUuidOfTemplateId();
	e = sqlite3_bind_blob(instance_stmt_, 1, &mu, mu.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_blob(instance_stmt_, 2, &tu, tu.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind template_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(instance_stmt_, 3, reinterpret_cast<const char *>(instance->label()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind label: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(instance_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(instance_stmt_);

	sqlite3_int64 rowid = sqlite3_last_insert_rowid(db_);
	instance->set_rowid(rowid);

	return true;
}

bool DatabaseDriver::SaveTargetModule(const Instance *instance, TargetModule *tm)
{
	if (!instance->IsSaved()) {
		std::cerr << "instance is not saved yet: "
			 << instance->module_id()
			 << std::endl;
		return false;
	}

	int e;
	e = sqlite3_bind_int64(tm_stmt_, 1, instance->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind instance_rowid: " << e << std::endl;
		return false;
	}
	boost::uuids::uuid u = tm->GetUuid();
	e = sqlite3_bind_blob(tm_stmt_, 2, &u, u.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(tm_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(tm_stmt_);

	sqlite3_int64 rowid = sqlite3_last_insert_rowid(db_);
	tm->set_rowid(rowid);

	return true;
}

bool DatabaseDriver::SaveTargetPq(const TargetModule *tm, const TargetPq *tpq)
{
	int e;
	e = sqlite3_bind_int64(tpq_stmt_, 1, tm->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind tm_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_int(tpq_stmt_, 2, tpq->pq_id());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind pq_id: " << e << std::endl;
		return false;
	}
	std::string math = tpq->GetString();
	e = sqlite3_bind_text(tpq_stmt_, 3, reinterpret_cast<const char *>(math.c_str()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind math: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(tpq_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(tpq_stmt_);
	return true;
}

bool DatabaseDriver::SaveTemplate(const Template *t)
{
	int e;
	boost::uuids::uuid tu = t->GetUuidOfTemplateId();
	boost::uuids::uuid rmu = t->GetUuidOfRefModuleId();
	e = sqlite3_bind_blob(template_stmt_, 1, &tu, tu.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind template_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_blob(template_stmt_, 2, &rmu, rmu.size(), SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind ref_module_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(template_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(template_stmt_);
	return true;
}

bool DatabaseDriver::SavePort(const Module *module, const Port *port)
{
	if (!module->IsSaved()) {
		std::cerr << "module is not saved yet: "
			 << module->module_id()
			 << std::endl;
		return false;
	}

	int e;
	e = sqlite3_bind_int64(port_stmt_, 1, module->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(port_stmt_, 2, reinterpret_cast<const char *>(port->port_id()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind port_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(port_stmt_, 3, reinterpret_cast<const char *>(port->direction()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind direction: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(port_stmt_, 4, reinterpret_cast<const char *>(port->ref_pq_id()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind ref_pq_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(port_stmt_, 5, reinterpret_cast<const char *>(port->multiple()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind multiple: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(port_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(port_stmt_);
	return true;
}

bool DatabaseDriver::SaveTimeseries(const Module *module, const Timeseries *ts,
									const boost::filesystem::path &given_path,
									const boost::filesystem::path &model_path)
{
	if (!module->IsSaved()) {
		std::cerr << "module is not saved yet: "
			 << module->module_id()
			 << std::endl;
		return false;
	}

	int e;
	e = sqlite3_bind_int64(timeseries_stmt_, 1, module->rowid());
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind module_rowid: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(timeseries_stmt_, 2, reinterpret_cast<const char *>(ts->timeseries_id()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind timeseries_id: " << e << std::endl;
		return false;
	}
	e = sqlite3_bind_text(timeseries_stmt_, 3, reinterpret_cast<const char *>(ts->format()), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind format: " << e << std::endl;
		return false;
	}

	boost::filesystem::path path;
	if (!GetAbsolutePathFromReference(ts, given_path, model_path, &path)) {
		std::cerr << "timeseries without reference" << std::endl;
		return false;
	}
	std::unique_ptr<char[]> utf8(GetUtf8FromPath(path));
	if (!utf8)
		return false;
	e = sqlite3_bind_text(timeseries_stmt_, 4, utf8.get(), -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		std::cerr << "failed to bind ref: " << e << std::endl;
		return false;
	}
	e = sqlite3_step(timeseries_stmt_);
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_reset(timeseries_stmt_);
	return true;
}

}
}
