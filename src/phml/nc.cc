/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.hh"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>

#include <boost/noncopyable.hpp>

#include "db/statement-driver.hh"
#include "phml.pb.h"

using std::cerr;
using std::endl;

namespace flint {
namespace phml {

namespace {

class Driver : boost::noncopyable {
public:
	Driver(sqlite3 *db, ::phml::NumericalConfiguration *nc)
		: nc_(nc)
		, nc_stmt_(NULL)
		, td_stmt_(NULL)
	{
		int e;
		e = sqlite3_prepare_v2(db, "SELECT * FROM ncs",
							   -1, &nc_stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
		e = sqlite3_prepare_v2(db, "SELECT unit_id, step FROM tds WHERE module_id IS NULL",
							   -1, &td_stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
	}

	~Driver() {
		sqlite3_finalize(nc_stmt_);
		sqlite3_finalize(td_stmt_);
	}

	bool Drive() {
		return SetPartOfNumericalConfiguration() && SetTimeDiscretization();
	}

private:
	bool SetPartOfNumericalConfiguration()
	{
		int e = sqlite3_step(nc_stmt_);
		if (e == SQLITE_DONE) { // no row
			return true;
		}
		if (e != SQLITE_ROW) { // error
			cerr << "failed to step prepared statement" << endl;
			return false;
		}
		const unsigned char *rg_name = sqlite3_column_text(nc_stmt_, 0);
		const unsigned char *rg_seed = sqlite3_column_text(nc_stmt_, 1);
		const unsigned char *integration = sqlite3_column_text(nc_stmt_, 2);
		int sts_unit_id = sqlite3_column_int(nc_stmt_, 3);
		const unsigned char *sts_value = sqlite3_column_text(nc_stmt_, 4);

		if (rg_name) {
			::phml::RandomGenerator *rg = nc_->mutable_rg();
			rg->set_name((const char *)rg_name);
			if (rg_seed) rg->set_seed((const char *)rg_seed);
		}
		if (integration) {
			nc_->set_integration((const char *)integration);
		}
		if (sts_unit_id > 0 && sts_value) {
			::phml::SimulationTimeSpan *sts = nc_->mutable_sts();
			sts->set_unit_id(sts_unit_id);
			sts->set_value((const char *)sts_value);
		}
		return true;
	}

	bool SetTimeDiscretization()
	{
		int e = sqlite3_step(td_stmt_);
		if (e == SQLITE_DONE) { // no row
			return true;
		}
		if (e != SQLITE_ROW) { // error
			cerr << "failed to step prepared statement" << endl;
			return false;
		}
		int unit_id = sqlite3_column_int(td_stmt_, 0);
		const unsigned char *step = sqlite3_column_text(td_stmt_, 1);
		if (unit_id > 0 && step) {
			::phml::TimeDiscretization *td = nc_->mutable_td();
			td->set_unit_id(unit_id);
			td->set_step((const char *)step);
		}
		return true;
	}

private:
	::phml::NumericalConfiguration *nc_;
	sqlite3_stmt *nc_stmt_;
	sqlite3_stmt *td_stmt_;
};

class MethodWriter : db::StatementDriver {
public:
	explicit MethodWriter(sqlite3 *db)
		: db::StatementDriver(db, "UPDATE config SET method = ?")
	{}

	bool Write(const char *method) {
		int e;
		e = sqlite3_bind_text(stmt(), 1, method, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind method: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

}

bool Nc(sqlite3 *db, const char *output, int *seed)
{
	std::unique_ptr<::phml::NumericalConfiguration> nc(new ::phml::NumericalConfiguration);
	{
		std::unique_ptr<Driver> driver(new Driver(db, nc.get()));
		if (!driver->Drive())
			return false;
	}
	{
		std::ofstream ofs(output, std::ios::binary);
		if (!ofs) {
			cerr << "failed to open " << output << endl;
			return false;
		}
		if (!nc->SerializeToOstream(&ofs)) {
			cerr << "failed to serialize NumericalConfiguration" << endl;
			return false;
		}
		ofs.close();
	}
	if (seed && nc->has_rg() && nc->rg().has_seed())
		*seed = std::atoi(nc->rg().seed().c_str());
	MethodWriter writer(db);
	if (nc->has_integration()) {
		if (!writer.Write(nc->integration().c_str()))
			return false;
	} else {
		// unspecified, so let's assume rk4
		if (!writer.Write("rk4"))
			return false;
	}
	return true;
}

}
}
