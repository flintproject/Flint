/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/task-runner.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <future>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "bc/index.h"
#include "cas/dimension.h"
#include "compiler.h"
#include "db/driver.h"
#include "db/read-only-driver.h"
#include "db/statement-driver.h"
#include "exec.h"
#include "exec/job-runner.h"
#include "exec/parameter.h"
#include "exec/progress.h"
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

bool TaskRunner::CreateControlFile(int num_samples)
{
	assert(task_);

	char filename[kFilenameLength];
	std::sprintf(filename, "%d/control", id_);
	size_t size = num_samples + 1;
	if (!workspace::CreateSparseFileAtomically(filename, size))
		return false;
	try {
		boost::interprocess::file_mapping fm(filename, boost::interprocess::read_write);
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
	char filename[kFilenameLength];
	std::sprintf(filename, "%d/progress", id_);
	size_t size = num_samples + 1;
	if (!workspace::CreateSparseFileAtomically(filename, size))
		return false;
	try {
		boost::interprocess::file_mapping fm(filename, boost::interprocess::read_write);
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
	char filename[kFilenameLength];
	std::sprintf(filename, "%d/rss", id_);
	size_t size = (num_samples + 1) * sizeof(double);
	if (!workspace::CreateSparseFileAtomically(filename, size))
		return false;
	try {
		boost::interprocess::file_mapping fm(filename, boost::interprocess::read_write);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_write);
		task_->rss_mr = std::move(mr);
	} catch (const boost::interprocess::interprocess_exception &e) {
		std::cerr << "failed to map file: " << e.what() << std::endl;
		return false;
	}
	return true;
}

bool TaskRunner::Setup(int id, const char *path, std::vector<double> *data)
{
	task_.reset(load::Load(path, load::kExec, id, data));
	if (!task_)
		return false;
	{
		db::Driver driver("x.db");
		auto db = driver.db();
		if (!db)
			return false;
		if (!task::Config(id, db))
			return false;
	}
	db::ReadOnlyDriver driver("x.db");
	auto db = driver.db();
	if (!db)
		return false;
	return CreateSpec(id, db);
}

bool TaskRunner::Run()
{
	if (!Setup(id_, path_.get(), &data_))
		return false;

	if (task_->IsCanceled())
		return true;

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
	// fill task's rest of fields
	task_->granularity = reader_->granularity();
	task_->output_start_time = reader_->output_start_time();
	{
		std::unique_ptr<Layout> layout(new Layout);
		LayoutLoader loader(layout_.get());
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

		if (reader_->GetDpsPath()) {
			auto ls_config = ls::CreateConfiguration(reader_->GetDpsPath(), *task_->layout);
			if (!ls_config)
				return false;
			task_->ls_config.swap(ls_config);
		}
	}
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
	return MonitorTaskProgress(v, &task_->progress_mr);
}

}
}
