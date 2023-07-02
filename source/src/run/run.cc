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

#include "cas.h"
#include "cas/dimension.h"
#include "compiler.h"
#include "db/driver.h"
#include "db/statement-driver.h"
#include "file.h"
#include "filter.h"
#include "filter/cutter.h"
#include "flint/ls.h"
#include "flint/utf8path.h"
#include "fppp.h"
#include "job.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "load.h"
#include "run/spec.h"
#include "runtime/channel.h"
#include "task.h"
#include "task/config-reader.h"

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

bool Run(const cli::RunOption &option, const boost::filesystem::path &dir)
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

	std::vector<double> data;
	std::unique_ptr<task::Task> task(load::Load(option.model_filename().c_str(), load::kRun, dir, &data));
	if (!task)
		return false;

	auto driver = db::Driver::Create(dir / "model.db");
	sqlite3 *db = driver->db();
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
		boost::filesystem::copy_file(spec_path, dir / "spec.txt", ec);
		if (ec) {
			std::cerr << "failed to copy "
					  << option.spec_filename()
					  << " to "
					  << dir
					  << "/spec.txt: "
					  << ec.message()
					  << std::endl;
			return false;
		}
	} else {
		// create the list of all variables
		boost::filesystem::ofstream ofs(dir / "spec.txt", std::ios::out|std::ios::binary);
		if (!ofs) {
			std::cerr << "failed to open "
					  << dir
					  << "/spec.txt"
					  << std::endl;
			return false;
		}
		bool r = Spec(db, &ofs);
		ofs.close();
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
	{
		task::ConfigReader reader(db);
		if (!reader.Read())
			return false;
		if (!filter::Create(db, dir / "spec.txt", dir / "layout", dir / "filter"))
			return false;
		if (!filter::Isdh(dir / "filter", dir / "isdh"))
			return false;
		task->method = reader.GetMethod();
		task->length = reader.length();
		task->step = reader.step();
		task->granularity = option.granularity();
		task->output_start_time = 0; // by default
		{
			std::unique_ptr<Layout> layout(new Layout);
			LayoutLoader loader(dir / "layout");
			if (!loader.Load(layout.get()))
				return false;
			size_t layer_size = layout->Calculate();
			assert(layer_size > kOffsetBase);
			task->layout.swap(layout);
			task->layer_size = layer_size;

			{
				filter::Cutter cutter;
				if (!cutter.Load(dir / "filter", layer_size))
					return false;
				task->writer.reset(cutter.CreateWriter());
			}
		}
		if (task->method == compiler::Method::kArk) {
			std::unique_ptr<cas::System> system(new cas::System);
			if (!system->Load(db))
				return false;
			if (!cas::AnnotateEquations(db, "input_eqs", system.get()))
				return false;
			task->system = std::move(system);
		} else {
			cas::DimensionAnalyzer da;
			if (!da.Load(db))
				return false;
			compiler::Compiler c(&da);
			task->bc.reset(c.Compile(db, "input_eqs", task->method));
			if (!task->bc)
				return false;
		}
	}
	std::unique_ptr<fppp::Option> fppp_option;
	if (option.has_fppp_host()) {
		fppp_option.reset(new fppp::Option);
		fppp_option->host = option.fppp_host().c_str();
		for (int i=0;i<option.fppp_output_size();i++) {
			key::Data kd;
			if (!key::Data::FromString(option.fppp_output(i), &kd)) {
				std::cerr << "invalid output name: "
						  << option.fppp_output(i)
						  << std::endl;
				return false;
			}
			fppp_option->output.emplace(kd, 0);
		}
		if (!runtime::LoadChannel(db, fppp_option->channel))
			return false;
	}
	boost::filesystem::path output_path = GetPathFromUtf8(option.output_filename().c_str());
	if (output_path.empty())
		return false;
	std::string output_file = output_path.string();
	return job::Job(0,
					nullptr,
					dir,
					dir / "0",
					*task,
					fppp_option.get(),
					&data,
					output_file.c_str()) != job::Result::kFailed;
}

}
}
