/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "compiler.hh"
#include "db/driver.h"
#include "db/statement-driver.h"
#include "file.hh"
#include "filter.hh"
#include "job.hh"
#include "load.hh"
#include "run/spec.hh"
#include "system.h"
#include "task.hh"
#include "task.hh"
#include "task/config-reader.hh"
#include "utf8path.h"
#include "workspace/task.h"

#include "cli.pb.h"

using std::cerr;
using std::endl;

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

bool Run(const char *input, int size)
{
	cli::RunOption option;
	if (!option.ParseFromArray(input, size)) {
		cerr << "failed to parse the input" << endl;
		return false;
	}
	workspace::Task task(option.model_filename().c_str());
	file::Format format;
	if (!task.Setup(&format)) {
		return false;
	}
	if (!load::Load(format, load::kRun))
		return false;

	db::Driver driver("modeldb");
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
		if (!Spec(db, fp))
			return false;
		fclose(fp);
	}
	// prepare granularity
	{
		GranularityWriter writer(db);
		if (option.has_granularity()) {
			if (!writer.Write(option.granularity()))
				return false;
		} else {
			if (!writer.Write(1))
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
	if (!compiler::Compile(db, "input_eqs", reader.GetCanonicalMethodName(), "bc"))
		return false;
	boost::filesystem::path output_path = GetPathFromUtf8(option.output_filename().c_str());
	std::string output_file = output_path.string();
	return job::Job(0, "init", output_file.c_str(), reader, db);
}

}
