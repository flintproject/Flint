/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <future>
#include <memory>
#include <system_error>
#include <thread>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>

#include "db/driver.h"
#include "db/read-only-driver.h"
#include "exec/task-runner.h"
#include "flint/background.h"
#include "flint/ctrl.h"
#include "flint/process.h"
#include "phsp.h"
#include "sedml.h"

namespace flint {
namespace exec {

namespace {

bool CreatePidTxt(const boost::filesystem::path &dir)
{
	boost::filesystem::ofstream ofs(dir / "pid.txt", std::ios::out|std::ios::binary);
	if (!ofs) {
		std::cerr << "failed to open " << dir << "/pid.txt" << std::endl;
		return false;
	}
	WriteCurrentProcessId(ofs);
	ofs.close();
	return true;
}

class FutureTaskPool {
public:
	FutureTaskPool(const boost::filesystem::path &dir, ctrl::Argument *arg)
		: dir_(dir)
		, arg_(arg)
	{}

	void Add(int id, const char *path) {
		size_t len = strlen(path);
		std::unique_ptr<char[]> p(new char[len+1]);
		if (len > 0)
			std::memcpy(p.get(), path, len);
		p[len] = '\0';
		auto lmbd = [this, id](char *p) {
			TaskRunner runner(id, p, dir_, arg_);
			return runner.Run();
		};
		tasks_.emplace_back(std::async(std::launch::async, lmbd, p.release()));
	}

	bool Wait() {
		bool result = true;
		// wait for all threads finishing regardless of their results
		for (auto &f : tasks_) {
			if (!f.get())
				result = false;
		}
		return result;
	}

private:
	const boost::filesystem::path &dir_;
	ctrl::Argument *arg_;
	std::vector<std::future<bool> > tasks_;
};

int PickTask(void *data, int argc, char **argv, char **names)
{
	FutureTaskPool *pool = static_cast<FutureTaskPool *>(data);
	(void)names;
	assert(argc == 2);
	int id = std::atoi(argv[0]);
	assert(id > 0);
	const char *path = argv[1];
	assert(path != nullptr);
	pool->Add(id, path);
	return 0;
}

bool CollectTasks(sqlite3 *db, FutureTaskPool *pool)
{
	char *em;
	int e;
	e = sqlite3_exec(db,
					 "SELECT t.rowid, m.absolute_path FROM tasks AS t"
					 " LEFT JOIN models AS m ON t.model_id = m.rowid"
					 " WHERE m.absolute_path IS NOT NULL",
					 &PickTask, pool, &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to exec: " << e
			 << ": " << em << std::endl;
		return false;
	}
	return true;
}

bool ReadInput(const cli::ExecOption &option, const boost::filesystem::path &dir)
{
	auto driver = db::Driver::Create(dir / "exec.db");
	sqlite3 *db = driver->db();
	if (!db)
		return false;
	return sedml::Read(option.sedml_filename().c_str(), db) &&
		phsp::Read(option.phsp_filename().c_str(), db, dir);
}

bool CopyInput(const boost::filesystem::path &dir)
{
	auto input_db = dir / "input.db";
	boost::system::error_code ec;
	boost::filesystem::rename(dir / "exec.db", input_db, ec);
	if (ec) {
		std::cerr << "failed to rename exec.db to input.db: "
				  << ec.message()
				  << std::endl;
		return false;
	}
	boost::filesystem::copy_file(input_db, dir / "x.db", ec);
	if (ec) {
		std::cerr << "failed to copy input.db to x.db: "
				  << ec.message()
				  << std::endl;
		return false;
	}
	return true;
}

bool RunTasks(const boost::filesystem::path &dir, ctrl::Argument *arg)
{
	FutureTaskPool pool(dir, arg);
	{
		auto driver = db::ReadOnlyDriver::Create(dir / "input.db");
		sqlite3 *db = driver->db();
		if (!db)
			return false;
		if (!CollectTasks(db, &pool))
			return false;
	}
	return pool.Wait();
}

}

bool Exec(const cli::ExecOption &option, const boost::filesystem::path &dir,
		  ctrl::Argument *arg)
{
	if (option.has_lock_filename())
		InitializeBackgroundProcess(option.lock_filename().c_str());
	return CreatePidTxt(dir) && ReadInput(option, dir) && CopyInput(dir) && RunTasks(dir, arg);
}

}
}
