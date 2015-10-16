/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/task-runner.hh"

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

#include "compiler.hh"
#include "db/driver.hh"
#include "db/read-only-driver.hh"
#include "exec.hh"
#include "exec/job-runner.hh"
#include "exec/progress.hh"
#include "filter.hh"
#include "job.hh"
#include "layout.hh"
#include "load.hh"
#include "task.hh"

using std::cerr;
using std::endl;
using std::sprintf;

namespace flint {
namespace exec {

namespace {

bool CreateSpec(int id, sqlite3 *db)
{
	char spec_file[64]; // large enough
	sprintf(spec_file, "%d/spec.txt", id);
	FILE *fp = fopen(spec_file, "w");
	if (!fp) {
		perror(spec_file);
		return false;
	}
	bool r = task::Spec(id, db, fp);
	fclose(fp);
	return r;
}

bool Setup(int id, const char *path)
{
	if (!load::Load(path, load::kExec, id))
		return false;
	{
		db::Driver driver("x.db");
		if (!task::Config(id, driver.db()))
			return false;
	}
	db::ReadOnlyDriver driver("x.db");
	return CreateSpec(id, driver.db());
}

const int kFilenameLength = 64;

}

TaskRunner::TaskRunner(int id, char *path)
	: id_(id)
	, path_(path)
	, dir_(new char[kFilenameLength])
	, layout_(new char[kFilenameLength])
	, generated_layout_(new char[kFilenameLength])
	, init_(new char[kFilenameLength])
{
	sprintf(dir_.get(), "%d", id);
	sprintf(layout_.get(), "%d/layout", id);
	sprintf(generated_layout_.get(), "%d/generated-layout", id);
	sprintf(init_.get(), "%d/init", id);
}

sqlite3 *TaskRunner::GetDatabase()
{
	return db_driver_->db();
}

sqlite3 *TaskRunner::GetModelDatabase()
{
	return modeldb_driver_->db();
}

void *TaskRunner::GetProgressAddress(int job_id)
{
	assert(progress_region_);
	assert(static_cast<size_t>(job_id) < progress_region_->get_size());
	char *addr = static_cast<char *>(progress_region_->get_address());
	return addr + job_id;
}

bool TaskRunner::Run()
{
	if (!Setup(id_, path_.get()))
		return false;

	{
		char canceled_file[kFilenameLength];
		sprintf(canceled_file, "%s/canceled", dir_.get());
		if (boost::filesystem::exists(canceled_file)) {
			// exit early if file "canceled" exists
			return true;
		}
	}

	char modeldb_file[kFilenameLength];
	sprintf(modeldb_file, "%s/model.db", dir_.get());
	modeldb_driver_.reset(new db::ReadOnlyDriver(modeldb_file));

	char spec_file[kFilenameLength];
	sprintf(spec_file, "%s/spec.txt", dir_.get());
	char filter_file[kFilenameLength];
	sprintf(filter_file, "%s/filter", dir_.get());
	if (!filter::Create(modeldb_driver_->db(), spec_file, layout_.get(), filter_file))
		return false;
	char track_file[kFilenameLength];
	sprintf(track_file, "%s/track", dir_.get());
	if (!filter::Track(filter_file, track_file))
		return false;
	char isdh_file[kFilenameLength];
	sprintf(isdh_file, "%s/isdh", dir_.get());
	if (!filter::Isdh(filter_file, isdh_file))
		return false;
	reader_.reset(new task::ConfigReader(modeldb_driver_->db()));
	if (!reader_->Read())
		return false;
	char bc_file[kFilenameLength];
	sprintf(bc_file, "%s/bc", dir_.get());
	if (!compiler::Compile(modeldb_driver_->db(), "input_eqs", reader_->GetCanonicalMethodName(), bc_file))
		return false;

	char db_file[kFilenameLength];
	sprintf(db_file, "%s/db", dir_.get());
	db_driver_.reset(new db::Driver(db_file));
	int n = exec::Enum(db_driver_->db());
	if (n == 0)
		return false;
	std::unique_ptr<boost::interprocess::file_mapping> fm(exec::CreateProgressFile(n, dir_.get()));
	if (!fm)
		return false;
	progress_region_.reset(new boost::interprocess::mapped_region(*fm, boost::interprocess::read_write));
	if (!task::Form(db_driver_->db()))
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
	std::unique_ptr<std::thread> th(exec::CreateTaskProgressThread(n,
																   progress_region_.get(),
																   &done));
	bool result = true;
	// wait for all threads finishing regardless of their results
	for (auto &f : v) {
		if (!f.get())
			result = false;
		done++;
	}
	th->join();
	return result;
}

}
}
