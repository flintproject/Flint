/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/task-runner.h"

#include <atomic>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <future>
#include <thread>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "cas/dimension.h"
#include "compiler.h"
#include "db/driver.h"
#include "db/read-only-driver.h"
#include "exec.h"
#include "exec/job-runner.h"
#include "exec/parameter.h"
#include "exec/progress.h"
#include "filter.h"
#include "job.h"
#include "layout.h"
#include "load.h"
#include "task.h"

namespace flint {
namespace exec {

namespace {

bool CreateSpec(int id, sqlite3 *db)
{
	char spec_file[64]; // large enough
	std::sprintf(spec_file, "%d/spec.txt", id);
	FILE *fp = fopen(spec_file, "w");
	if (!fp) {
		perror(spec_file);
		return false;
	}
	bool r = task::Spec(id, db, fp);
	fclose(fp);
	return r;
}

const int kFilenameLength = 64;

}

TaskRunner::TaskRunner(int id, char *path)
	: id_(id)
	, path_(path)
	, dir_(new char[kFilenameLength])
	, layout_(new char[kFilenameLength])
	, generated_layout_(new char[kFilenameLength])
{
	std::sprintf(dir_.get(), "%d", id);
	std::sprintf(layout_.get(), "%d/layout", id);
	std::sprintf(generated_layout_.get(), "%d/generated-layout", id);
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

void *TaskRunner::GetProgressAddress(int job_id)
{
	assert(progress_region_);
	assert(static_cast<size_t>(job_id) < progress_region_->get_size());
	char *addr = static_cast<char *>(progress_region_->get_address());
	return addr + job_id;
}

bool TaskRunner::Setup(int id, const char *path, std::vector<double> *data)
{
	task_.reset(load::Load(path, load::kExec, id, data));
	if (!task_)
		return false;
	{
		db::Driver driver("x.db");
		if (!task::Config(id, driver.db()))
			return false;
	}
	db::ReadOnlyDriver driver("x.db");
	return CreateSpec(id, driver.db());
}

bool TaskRunner::Run()
{
	if (!Setup(id_, path_.get(), &data_))
		return false;

	{
		char canceled_file[kFilenameLength];
		std::sprintf(canceled_file, "%s/canceled", dir_.get());
		if (boost::filesystem::exists(canceled_file)) {
			// exit early if file "canceled" exists
			return true;
		}
	}

	char modeldb_file[kFilenameLength];
	std::sprintf(modeldb_file, "%s/model.db", dir_.get());
	modeldb_driver_.reset(new db::ReadOnlyDriver(modeldb_file));

	char spec_file[kFilenameLength];
	std::sprintf(spec_file, "%s/spec.txt", dir_.get());
	char filter_file[kFilenameLength];
	std::sprintf(filter_file, "%s/filter", dir_.get());
	if (!filter::Create(modeldb_driver_->db(), spec_file, layout_.get(), filter_file))
		return false;
	char track_file[kFilenameLength];
	std::sprintf(track_file, "%s/track", dir_.get());
	if (!filter::Track(filter_file, track_file))
		return false;
	char isdh_file[kFilenameLength];
	std::sprintf(isdh_file, "%s/isdh", dir_.get());
	if (!filter::Isdh(filter_file, isdh_file))
		return false;
	reader_.reset(new task::ConfigReader(modeldb_driver_->db()));
	if (!reader_->Read())
		return false;
	if (reader_->GetMethod() != compiler::Method::kArk) {
		cas::DimensionAnalyzer da;
		if (!da.Load(modeldb_driver_->db()))
			return false;
		compiler::Compiler c(&da);
		task_->reinit_bc.reset(c.Compile(modeldb_driver_->db(), "dependent_ivs", compiler::Method::kAssign));
		if (!task_->reinit_bc)
			return false;
		task_->bc.reset(c.Compile(modeldb_driver_->db(), "input_eqs", reader_->GetMethod()));
		if (!task_->bc)
			return false;
	}

	char db_file[kFilenameLength];
	std::sprintf(db_file, "%s/task.db", dir_.get());
	db_driver_.reset(new db::Driver(db_file));
	if (!exec::SaveParameters(id_, db_driver_->db()))
		return false;
	int n = exec::Enum(db_driver_->db());
	if (n == 0)
		return false;
	std::unique_ptr<boost::interprocess::file_mapping> fm(exec::CreateProgressFile(n, dir_.get()));
	if (!fm)
		return false;
	progress_region_.reset(new boost::interprocess::mapped_region(*fm, boost::interprocess::read_write));
	if (!task::Form(db_driver_->db()))
		return false;
	dimension_analyzer_.reset(new cas::DimensionAnalyzer);
	if (!dimension_analyzer_->Load(db_driver_->db()))
		return false;
	if (!layout::Generate(db_driver_->db(), generated_layout_.get()))
		return false;
	std::vector<std::future<bool> > v;
	for (;;) {
		int job_id;
		if (!job::Generate(db_driver_->db(), dir_.get(), &job_id))
			return false;
		if (job_id == 0)
			break; // all jobs has been generated

		auto lmbd = [](TaskRunner *tr, int id){
			JobRunner runner(tr, id);
			return runner.Run();
		};
		v.emplace_back(std::async(std::launch::async, lmbd, this, job_id));
	}
	assert(static_cast<size_t>(n) == v.size());
	std::atomic<size_t> done(0);
	std::thread th(exec::CreateTaskProgressThread(n,
												  progress_region_.get(),
												  &done));
	bool result = true;
	// wait for all threads finishing regardless of their results
	for (auto &f : v) {
		if (!f.get())
			result = false;
		done++;
	}
	th.join();
	return result;
}

}
}
