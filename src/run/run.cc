/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run.h"

#include <cassert>
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
#include "flint/ls.h"
#include "fppp.h"
#include "job.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "load.h"
#include "run/spec.h"
#include "task.h"
#include "task/config-reader.h"
#include "utf8path.h"

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
			std::cerr << "failed to bind granularity: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
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
		if (error_path.empty())
			return false;
		std::string error_file = error_path.string();
		if (!std::freopen(error_file.c_str(), "w", stderr)) {
			std::perror(error_file.c_str());
			return false;
		}
	}

	if (option.has_lock_filename())
		InitializeBackgroundProcess(option.lock_filename().c_str());

	std::vector<double> data;
	std::unique_ptr<task::Task> task(load::Load(option.model_filename().c_str(), load::kRun, 0, &data));
	if (!task)
		return false;

	db::Driver driver("model.db");
	sqlite3 *db = driver.db();
	if (!db)
		return false;
	// prepare spec.txt in both cases
	if (option.has_spec_filename()) {
		boost::filesystem::path spec_path = GetPathFromUtf8(option.spec_filename().c_str());
		if (spec_path.empty())
			return false;
		boost::system::error_code ec;
		if (!boost::filesystem::is_regular_file(spec_path, ec) || ec) {
			std::cerr << ec.message() << std::endl;
			return false;
		}
		boost::filesystem::copy_file(spec_path, "spec.txt", ec);
		if (ec) {
			std::cerr << "failed to copy "
				 << option.spec_filename()
				 << " to spec.txt: "
				 << ec.message()
				 << std::endl;
			return false;
		}
	} else {
		// create the list of all variables
		FILE *fp = fopen("spec.txt", "w");
		if (!fp) {
			std::perror("spec.txt");
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
	{
		std::unique_ptr<Layout> layout(new Layout);
		LayoutLoader loader("layout");
		if (!loader.Load(layout.get()))
			return false;
		size_t layer_size = layout->Calculate();
		assert(layer_size > kOffsetBase);
		task->layout.swap(layout);
		task->layer_size = layer_size;
	}
	if (reader.GetMethod() != compiler::Method::kArk) {
		cas::DimensionAnalyzer da;
		if (!da.Load(db))
			return false;
		compiler::Compiler c(&da);
		task->bc.reset(c.Compile(db, "input_eqs", reader.GetMethod()));
		if (!task->bc)
			return false;
	}
	std::unique_ptr<fppp::Option> fppp_option;
	if (option.has_fppp_host()) {
		fppp_option.reset(new fppp::Option);
		fppp_option->host = option.fppp_host().c_str();
		for (int i=0;i<option.fppp_output_size();i++) {
			fppp::KeyData kd;
			if (!fppp::KeyData::FromString(option.fppp_output(i), &kd)) {
				std::cerr << "invalid output name: "
						  << option.fppp_output(i)
						  << std::endl;
				return false;
			}
			fppp_option->output.emplace(kd, 0);
		}
	}
	boost::filesystem::path output_path = GetPathFromUtf8(option.output_filename().c_str());
	if (output_path.empty())
		return false;
	std::string output_file = output_path.string();
	return job::Job(".",
					"0",
					*task,
					nullptr,
					fppp_option.get(),
					&data,
					output_file.c_str(),
					reader,
					db);
}

}
}
