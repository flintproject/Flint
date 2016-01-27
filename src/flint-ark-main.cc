/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "bc/binary.h"
#include "cas/dimension.h"
#include "compiler.hh"
#include "db/driver.hh"
#include "db/statement-driver.hh"
#include "file.hh"
#include "filter.hh"
#include "job.hh"
#include "load.hh"
#include "run/spec.hh"
#include "solver.h"
#include "system.h"
#include "task.hh"
#include "task/config-reader.hh"
#include "utf8path.h"

#include "cli.pb.h"

using std::cerr;
using std::endl;
using std::perror;
using std::strcmp;

using namespace flint;

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

bool Run(const char *input, int size)
{
	cli::RunOption option;
	if (!option.ParseFromArray(input, size)) {
		cerr << "failed to parse the input" << endl;
		return false;
	}

	// redirect errors if requested
	if (option.has_error_filename()) {
		boost::filesystem::path error_path = GetPathFromUtf8(option.error_filename().c_str());
		std::string error_file = error_path.string();
		if (!std::freopen(error_file.c_str(), "w", stderr)) {
			perror(error_file.c_str());
			return false;
		}
	}

	if (!load::Load(option.model_filename().c_str(), load::kRun))
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
		if (!run::Spec(db, fp))
			return false;
		fclose(fp);
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
	{
		cas::DimensionAnalyzer da;
		if (!da.Load(db))
			return false;
		compiler::Compiler c(&da);
		if (!c.Compile(db, "input_eqs", reader.GetMethod(), "bc"))
			return false;
	}
	boost::filesystem::path output_path = GetPathFromUtf8(option.output_filename().c_str());
	std::string output_file = output_path.string();

	solver::Option opt;
	opt.end = 100;
	opt.dt = 0.01;
	opt.layout_file = "layout";
	opt.input_data_file = "init";
	opt.filter_file = "filter";
	opt.granularity = option.granularity(); // TODO
	FILE *fp = std::fopen(output_file.c_str(), "wb");
	if (!fp) {
		std::perror(output_file.c_str());
		return false;
	}
	opt.output_fp = fp;
	bool r = solver::Solve(db, solver::Method::kArk, opt);
	std::fclose(fp);
	return r;
}

const size_t kInputLength = 8192;

void Usage()
{
	cerr << "usage: flint-ark" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if ( strcmp(argv[1], "-h") == 0 ||
			 strcmp(argv[1], "--help") == 0 ) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}

	RequestBinaryStdio();
	// read input parameters from stdin
	char buffer[kInputLength];
	size_t s = std::fread(buffer, 1, kInputLength, stdin);
	if (s == 0) {
		cerr << "failed to read the input" << endl;
		return EXIT_FAILURE;
	}
	return Run(buffer, (int)s) ? EXIT_SUCCESS : EXIT_FAILURE;
}
