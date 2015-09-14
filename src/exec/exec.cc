/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec.hh"

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

#include "db/driver.hh"
#include "exec/task-runner.hh"
#include "phsp.hh"
#include "sedml.hh"

using std::cerr;
using std::endl;
using std::memcpy;

namespace flint {
namespace exec {

namespace {

class FutureTaskPool {
public:
	void Add(int id, const char *path) {
		size_t len = strlen(path);
		std::unique_ptr<char[]> p(new char[len+1]);
		if (len > 0)
			memcpy(p.get(), path, len);
		p[len] = '\0';
		auto lmbd = [id](char *p){
			TaskRunner runner(id, p);
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
		cerr << "failed to exec: " << e
			 << ": " << em << endl;
		return false;
	}
	return true;
}

bool RunTasks(sqlite3 *db)
{
	FutureTaskPool pool;
	if (!CollectTasks(db, &pool))
		return false;
	return pool.Wait();
}

}

bool Exec(const char *sedml_file, const char *phsp_file)
{
	db::Driver driver("x.db");
	sqlite3 *db = driver.db();
	if (!sedml::Read(sedml_file, db))
		return false;
	if (!phsp::Read(phsp_file, db))
		return false;
	return RunTasks(db);
}

}
}
