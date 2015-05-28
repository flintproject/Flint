/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>

#include <boost/scoped_ptr.hpp>

#include "phml.pb.h"

#include "bc/binary.h"
#include "db/read-only-driver.hh"
#include "sqlite3.h"

using std::cerr;
using std::endl;
using std::strcmp;

namespace {

class Driver : db::ReadOnlyDriver {
public:
	Driver(const char *filename, phml::NumericalConfiguration *nc)
		: db::ReadOnlyDriver(filename)
		, nc_(nc)
		, nc_stmt_(NULL)
		, td_stmt_(NULL)
	{
		int e;
		e = sqlite3_prepare_v2(db(), "SELECT * FROM ncs",
							   -1, &nc_stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
		e = sqlite3_prepare_v2(db(), "SELECT unit_id, step FROM tds WHERE module_id IS NULL",
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
			phml::RandomGenerator *rg = nc_->mutable_rg();
			rg->set_name((const char *)rg_name);
			if (rg_seed) rg->set_seed((const char *)rg_seed);
		}
		if (integration) {
			nc_->set_integration((const char *)integration);
		}
		if (sts_unit_id > 0 && sts_value) {
			phml::SimulationTimeSpan *sts = nc_->mutable_sts();
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
			phml::TimeDiscretization *td = nc_->mutable_td();
			td->set_unit_id(unit_id);
			td->set_step((const char *)step);
		}
		return true;
	}

private:
	phml::NumericalConfiguration *nc_;
	sqlite3_stmt *nc_stmt_;
	sqlite3_stmt *td_stmt_;
};

void Usage() {
	cerr << "usage: flint-nc DB [METHOD]" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	if (argc < 2 || 3 < argc) {
		Usage();
		return EXIT_FAILURE;
	}
	if ( strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0 ) {
		Usage();
		return EXIT_SUCCESS;
	}

	RequestBinaryStdio();

	boost::scoped_ptr<phml::NumericalConfiguration> nc(new phml::NumericalConfiguration);
	{
		boost::scoped_ptr<Driver> driver(new Driver(argv[1], nc.get()));
		if (!driver->Drive()) return EXIT_FAILURE;
	}
	if (!nc->SerializeToOstream(&std::cout)) {
		cerr << "failed to serialize NumericalConfiguration" << endl;
		return EXIT_FAILURE;
	}

	if (argc == 3) {
		std::ofstream ofs(argv[2]);
		if (!ofs.is_open()) {
			cerr << "failed to open method file: " << argv[2] << endl;
			return EXIT_FAILURE;
		}
		if (nc->has_integration()) {
			ofs << nc->integration() << endl;
		} else {
			ofs << "unspecified" << endl;
		}
		ofs.close();
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
