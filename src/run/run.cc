/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "cas/dimension.h"
#include "compiler.h"
#include "db/driver.h"
#include "db/statement-driver.h"
#include "file.h"
#include "filter.h"
#include "flint/background.h"
#include "job.h"
#include "load.h"
#include "run/spec.h"
#include "system.h"
#include "task.h"
#include "task/config-reader.h"
#include "utf8path.h"

using std::cerr;
using std::endl;
using std::perror;

namespace flint {
namespace run {

namespace {

class GranularityWriter : db::StatementDriver {
public:
	explicit GranularityWriter(sqlite3 *db)
		: db::StatementDriver(db, "UPDATE config SET granularity = ?")
	{}

	bool Write(int granularity) {
		int e;
		e = sqlite3_bind_int(stmt(), 1, granularity);
		if (e != SQLITE_OK) {
			cerr << "failed to bind granularity: " << e << endl;
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

bool Run(const cli::RunOption &option)
{
	// redirect errors if requested
	if (option.has_error_filename()) {
		boost::filesystem::path error_path = GetPathFromUtf8(option.error_filename().c_str());
		std::string error_file = error_path.string();
		if (!std::freopen(error_file.c_str(), "w", stderr)) {
			perror(error_file.c_str());
			return false;
		}
	}

	if (option.has_lock_filename())
		InitializeBackgroundProcess(option.lock_filename().c_str());

	std::vector<double> data;
	if (!load::Load(option.model_filename().c_str(), load::kRun, 0, &data))
		return false;

	db::Driver driver("model.db");
	sqlite3 *db = driver.db();
	// prepare spec.txt in both cases
	if (option.has_spec_filename()) {
		boost::filesystem::path spec_path = GetPathFromUtf8(option.spec_filename().c_str());
		boost::system::error_code ec;
		if (!boost::filesystem::is_regular_file(spec_path, ec) || ec) {
			cerr << ec.message() << endl;
			return false;
		}
		boost::filesystem::copy_file(spec_path, "spec.txt", ec);
		if (ec) {
			cerr << "failed to copy "
				 << option.spec_filename()
				 << " to spec.txt: "
				 << ec.message()
				 << endl;
			return false;
		}
	} else {
		// create the list of all variables
		FILE *fp = fopen("spec.txt", "w");
		if (!fp) {
			perror("spec.txt");
			return false;
		}
		bool r = Spec(db, fp);
		std::fclose(fp);
		if (!r)
			return false;
	}
	// prepare granularity
	{
		GranularityWriter writer(db);
		if (option.has_granularity()) {
			if (!writer.Write(option.granularity()))
				return false;
		}
	}
	task::ConfigReader reader(db);
	if (!reader.Read())
		return false;
	if (!filter::Create(db, "spec.txt", "layout", "filter"))
		return false;
	if (!filter::Isdh("filter", "isdh"))
		return false;
	if (reader.GetMethod() != compiler::Method::kArk) {
		cas::DimensionAnalyzer da;
		if (!da.Load(db))
			return false;
		compiler::Compiler c(&da);
		if (!c.Compile(db, "input_eqs", reader.GetMethod(), "bc"))
			return false;
	}
	boost::filesystem::path output_path = GetPathFromUtf8(option.output_filename().c_str());
	std::string output_file = output_path.string();
	return job::Job(".", "0", nullptr, &data, output_file.c_str(), reader, db);
}

}
}
