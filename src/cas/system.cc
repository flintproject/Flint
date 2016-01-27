/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas.h"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <sstream>

#include <boost/uuid/uuid_io.hpp>

#include "cas/dimension.h"
#include "cas/icg.h"
#include "cas/printer.h"
#include "compiler.hh"
#include "db/driver.hh"
#include "db/query.h"

using std::cerr;
using std::endl;

namespace flint {
namespace cas {

System::System() = default;

System::~System() = default;

bool System::Load(sqlite3 *db)
{
	da_.reset(new DimensionAnalyzer);
	printer_.reset(new Printer);
	return da_->Load(db) && printer_->Load(db);
}

void System::Add(const boost::uuids::uuid &uuid, Ode ode)
{
	uuids_.insert(uuid);
	odes_[uuid].emplace_back(ode);
}

void System::Add(const boost::uuids::uuid &uuid, Def def)
{
	uuids_.insert(uuid);
	defs_[uuid].emplace_back(def);
}

bool System::GenerateAuxVarAst(sqlite3 *db)
{
	if (!SaveNol(1, db))
		return false;
	if (!CreateAsts(db))
		return false;
	icg::AuxVar aux_var(printer_.get(), db);
	for (const auto &uuid : uuids_) {
		auto dit = defs_.find(uuid);
		if (dit != defs_.end()) {
			for (const auto &def : dit->second) {
				if (!aux_var.Insert(uuid, def))
					return false;
			}
		}
		auto oit = odes_.find(uuid);
		if (oit != odes_.end()) {
			for (const auto &ode : oit->second) {
				if (!aux_var.Insert(uuid, ode))
					return false;
			}
		}
	}
	return true;
}

bool System::SaveAuxVarBc(const char *file)
{
	db::Driver driver(":memory:");
	if (!GenerateAuxVarAst(driver.db()))
		return false;
	compiler::Compiler c(da_.get());
	return c.GenerateBytecode(driver.db(), file);
}

bool System::GenerateOdeMassAst(sqlite3 *db)
{
	if (!SaveNol(1, db))
		return false;
	if (!CreateAsts(db))
		return false;
	icg::OdeMass ode_mass(printer_.get(), db);
	for (const auto &uuid : uuids_) {
		auto dit = defs_.find(uuid);
		if (dit != defs_.end()) {
			for (const auto &def : dit->second) {
				if (!ode_mass.Insert(uuid, def))
					return false;
			}
		}
		auto oit = odes_.find(uuid);
		if (oit != odes_.end()) {
			for (const auto &ode : oit->second) {
				if (!ode_mass.Insert(uuid, ode))
					return false;
			}
		}
	}
	return true;
}

bool System::SaveOdeMassBc(const char *file)
{
	db::Driver driver(":memory:");
	if (!GenerateOdeMassAst(driver.db()))
		return false;
	compiler::Compiler c(da_.get());
	return c.GenerateBytecode(driver.db(), file);
}

bool System::GenerateOdeRhsAst(sqlite3 *db)
{
	if (!SaveNol(2, db))
		return false;
	if (!CreateAsts(db))
		return false;
	icg::OdeRhs ode_rhs(printer_.get(), db);
	for (const auto &uuid : uuids_) {
		auto dit = defs_.find(uuid);
		if (dit != defs_.end()) {
			for (const auto &def : dit->second) {
				if (!ode_rhs.Insert(uuid, def))
					return false;
			}
		}
		auto oit = odes_.find(uuid);
		if (oit != odes_.end()) {
			for (const auto &ode : oit->second) {
				if (!ode_rhs.Insert(uuid, ode))
					return false;
			}
		}
	}
	return true;
}

bool System::SaveOdeRhsBc(const char *file)
{
	db::Driver driver(":memory:");
	if (!GenerateOdeRhsAst(driver.db()))
		return false;
	compiler::Compiler c(da_.get());
	return c.GenerateBytecode(driver.db(), file);
}

bool System::FindMass(const boost::uuids::uuid &uuid, const std::string &name,
					  std::string *found) const
{
	auto it = odes_.find(uuid);
	if (it == odes_.end()) {
		cerr << "failed to find mass matrix: " << uuid << endl;
		return false;
	}
	for (const auto &ode : it->second) {
		assert(ode.name().size() > 1);
		assert(ode.name().at(0) == '%');
		if (ode.name().substr(1) == name) {
			int w = ode.mass().which();
			if (w == kExprIsInteger) {
				assert(boost::get<int>(ode.mass()) == 1);
				*found = "";
				return true;
			} else if (w == kExprIsString) {
				*found = boost::get<std::string>(ode.mass());
				return true;
			}
			cerr << "found unsupported form of mass matrix: "
				 << uuid
				 << ": "
				<< name
				 << endl;
			return false;
		}
	}
	cerr << "failed to find mass matrix: "
		 << uuid
		 << ": "
		 << name
		 << endl;
	return false;
}

}
}
