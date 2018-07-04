/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/task-runner.h"

#include <cassert>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <future>
#include <numeric>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>
#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

#include "bc/index.h"
#include "cas.h"
#include "cas/dimension.h"
#include "compiler.h"
#include "db/driver.h"
#include "db/read-only-driver.h"
#include "db/statement-driver.h"
#include "exec.h"
#include "exec/job-runner.h"
#include "exec/parameter.h"
#include "filter.h"
#include "filter/cutter.h"
#include "flint/ls.h"
#include "flint/workspace.h"
#include "job.h"
#include "layout.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "load.h"
#include "task.h"
#include "task/config-reader.h"

namespace flint {
namespace exec {

namespace {

int CountParameterSamples(sqlite3 *db)
{
	db::StatementDriver sd(db, "SELECT COUNT(*) FROM parameter_samples");
	int e = sqlite3_step(sd.stmt());
	if (e != SQLITE_ROW) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return -1;
	}
	return sqlite3_column_int(sd.stmt(), 0);
}

const unsigned int kUnitInterval = 200;
const unsigned int kMaxInterval = kUnitInterval << 5;

}

TaskRunner::TaskRunner(int id, char *path, const boost::filesystem::path &dir,
					   ctrl::Argument *arg)
	: id_(id)
	, path_(path)
	, arg_(arg)
	, dir_(dir)
	, layout_(dir)
	, generated_layout_(dir)
{
	char buf[64];
	std::sprintf(buf, "%d", id);
	dir_ /= buf;
	std::sprintf(buf, "%d/layout", id);
	layout_ /= buf;
	std::sprintf(buf, "%d/generated-layout", id);
	generated_layout_ /= buf;
}

TaskRunner::~TaskRunner() = default;

task::Task *TaskRunner::GetTask()
{
	return task_.get();
}

sqlite3 *TaskRunner::GetDatabase()
{
	return db_driver_->db();
}

sqlite3 *TaskRunner::GetModelDatabase()
{
	return modeldb_driver_->db();
}

const cas::DimensionAnalyzer *TaskRunner::GetDimensionAnalyzer() const
{
	return dimension_analyzer_.get();
}

bool TaskRunner::CreateSpec(int id, sqlite3 *db)
{
	auto spec_file = dir_ / "spec.txt";
	boost::filesystem::ofstream ofs(spec_file, std::ios::out|std::ios::binary);
	if (!ofs) {
		std::cerr << "failed to open " << spec_file << std::endl;
		return false;
	}
	bool r = task::Spec(id, db, ofs);
	ofs.close();
	return r;
}

bool TaskRunner::CreateControlFile(int num_samples)
{
	assert(task_);

	auto control_file = dir_ / "control";
	size_t size = num_samples + 1;
	if (!workspace::CreateSparseFileAtomically(control_file, size))
		return false;
	try {
		boost::interprocess::file_mapping fm(control_file.string().c_str(), boost::interprocess::read_write);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_write);
		task_->control_mr = std::move(mr);
	} catch (const boost::interprocess::interprocess_exception &e) {
		std::cerr << "failed to map file: " << e.what() << std::endl;
		return false;
	}
	return true;
}

bool TaskRunner::CreateProgressFile(int num_samples)
{
	auto progress_file = dir_ / "progress";
	size_t size = num_samples + 1;
	if (!workspace::CreateSparseFileAtomically(progress_file, size))
		return false;
	try {
		boost::interprocess::file_mapping fm(progress_file.string().c_str(), boost::interprocess::read_write);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_write);
		task_->progress_mr = std::move(mr);
	} catch (const boost::interprocess::interprocess_exception &e) {
		std::cerr << "failed to map file: " << e.what() << std::endl;
		return false;
	}
	return true;
}

bool TaskRunner::CreateRssFile(int num_samples)
{
	auto rss_file = dir_ / "rss";
	size_t size = (num_samples + 1) * sizeof(double);
	if (!workspace::CreateSparseFileAtomically(rss_file, size))
		return false;
	try {
		boost::interprocess::file_mapping fm(rss_file.string().c_str(), boost::interprocess::read_write);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_write);
		task_->rss_mr = std::move(mr);
	} catch (const boost::interprocess::interprocess_exception &e) {
		std::cerr << "failed to map file: " << e.what() << std::endl;
		return false;
	}
	return true;
}

bool TaskRunner::Run(int concurrency)
{
	task_.reset(load::Load(path_.get(), load::kExec, dir_, &data_));
	if (!task_)
		return false;
	{
		auto driver = db::Driver::Create(dir_.parent_path() / "x.db");
		auto db = driver->db();
		if (!db)
			return false;
		if (!task::Config(id_, db, dir_))
			return false;
	}
	{
		auto driver = db::ReadOnlyDriver::Create(dir_.parent_path() / "x.db");
		auto db = driver->db();
		if (!db)
			return false;
		if (!CreateSpec(id_, db))
			return false;
	}

	modeldb_driver_ = db::ReadOnlyDriver::Create(dir_ / "model.db");
	auto spec_file = dir_ / "spec.txt";
	auto filter_file = dir_ / "filter";
	if (!filter::Create(modeldb_driver_->db(), spec_file, layout_, filter_file))
		return false;
	auto track_file = dir_ / "track";
	if (!filter::Track(filter_file, track_file))
		return false;
	auto isdh_file = dir_ / "isdh";
	if (!filter::Isdh(filter_file, isdh_file))
		return false;
	{
		task::ConfigReader reader(modeldb_driver_->db());
		if (!reader.Read())
			return false;
		// fill task's rest of fields
		task_->method = reader.GetMethod();
		task_->length = reader.length();
		task_->step = reader.step();
		task_->granularity = reader.granularity();
		task_->output_start_time = reader.output_start_time();
		{
			std::unique_ptr<Layout> layout(new Layout);
			LayoutLoader loader(layout_);
			if (!loader.Load(layout.get()))
				return false;
			size_t layer_size = layout->Calculate();
			assert(layer_size > kOffsetBase);
			task_->layout.swap(layout);
			task_->layer_size = layer_size;

			{
				filter::Cutter cutter;
				if (!cutter.Load(filter_file, layer_size))
					return false;
				task_->writer.reset(cutter.CreateWriter());
			}

			if (reader.GetDpsPath()) {
				auto ls_config = ls::CreateConfiguration(reader.GetDpsPath(), *task_->layout);
				if (!ls_config)
					return false;
				task_->ls_config.swap(ls_config);
			}
		}
	}
	if (task_->method == compiler::Method::kArk) {
		std::unique_ptr<cas::System> system(new cas::System);
		if (!system->Load(modeldb_driver_->db()))
			return false;
		if (!cas::AnnotateEquations(modeldb_driver_->db(), "input_eqs", system.get()))
			return false;
		task_->system = std::move(system);
	} else {
		cas::DimensionAnalyzer da;
		if (!da.Load(modeldb_driver_->db()))
			return false;
		compiler::Compiler c(&da);
		task_->reinit_bc.reset(c.Compile(modeldb_driver_->db(), "dependent_ivs", compiler::Method::kAssign));
		if (!task_->reinit_bc)
			return false;
		task_->bc.reset(c.Compile(modeldb_driver_->db(), "input_eqs", task_->method));
		if (!task_->bc)
			return false;
	}

	db_driver_ = db::Driver::Create(dir_ / "task.db");
	if (!exec::SaveParameters(dir_, db_driver_->db()))
		return false;
	int n = CountParameterSamples(db_driver_->db());
	if (n <= 0)
		return false;
	if (!CreateControlFile(n))
		return false;
	if (!CreateProgressFile(n))
		return false;
	if (task_->ls_config && !CreateRssFile(n))
		return false;
	if (!task::Form(db_driver_->db()))
		return false;
	dimension_analyzer_.reset(new cas::DimensionAnalyzer);
	if (!dimension_analyzer_->Load(db_driver_->db()))
		return false;
	if (!layout::Generate(db_driver_->db(), generated_layout_))
		return false;
	std::vector<std::future<bool> > v;
	size_t c = static_cast<size_t>((concurrency > 0) ? concurrency : 1);
	char *addr = static_cast<char *>(task_->progress_mr.get_address());
	auto reflect = [addr, n]{
		// addr is 1-based
		auto a = std::accumulate<char *, int64_t>(addr+1, addr+1+n, 0)/n; // TODO: replace with C++17's std::reduce
		assert(0 <= a && a <= 100);
		*addr = static_cast<char>(a);
	};
	bool generated_all = false;
	unsigned int interval = kUnitInterval;
	bool result = true;
	do {
		while (!generated_all && v.size() < c) {
			int job_id;
			if (!job::Generate(db_driver_->db(), dir_, &job_id))
				return false;
			if (job_id == 0) { // all jobs has been generated
				generated_all = true;
				break;
			}

			auto lmbd = [](TaskRunner *tr, int id){
				JobRunner runner(tr, id);
				return runner.Run();
			};
			v.emplace_back(std::async(std::launch::async, lmbd, this, job_id));
		}

		auto it = v.begin();
		while (it != v.end()) {
			auto &f = *it;
			std::future_status s = f.wait_for(std::chrono::milliseconds(0));
			switch (s) {
			case std::future_status::deferred:
				++it; // come back later
				break;
			case std::future_status::timeout:
				++it;
				break;
			case std::future_status::ready:
				if (!f.get())
					result = false;
				interval = 0;
				it = v.erase(it);
				break;
			}
		}

		reflect();

		if (generated_all && v.empty())
			goto done;
		if (interval == 0) {
			interval = kUnitInterval;
		} else {
			// wait for a job finished
			std::this_thread::sleep_for(std::chrono::milliseconds(interval));
			if (interval < kMaxInterval)
				interval <<= 1;
		}
	} while (!task_->IsCanceled());
	reflect();
 done:
	return result;
}

}
}
