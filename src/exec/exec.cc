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
#include <system_error>
#include <thread>
#include <vector>

#include "db/driver.h"
#include "exec/task-runner.hh"
#include "phsp.hh"
#include "sedml.hh"

using std::cerr;
using std::endl;

namespace exec {

namespace {

typedef std::vector<std::future<bool> > FutureTasks;

int PickTask(void *data, int argc, char **argv, char **names)
{
	FutureTasks *v = static_cast<FutureTasks *>(data);
	(void)names;
	assert(argc == 1);
	int id = std::atoi(argv[0]);
	assert(id > 0);
	auto lmbd = [id](){
		TaskRunner runner(id);
		return runner.Run();
	};
	v->emplace_back(std::async(std::launch::async, lmbd));
	return 0;
}

bool CollectTasks(sqlite3 *db, FutureTasks *v)
{
	char *em;
	int e;
	e = sqlite3_exec(db,
					 "SELECT t.rowid FROM tasks AS t WHERE EXISTS (SELECT * FROM models AS m WHERE t.model_id = m.rowid)",
					 &PickTask, v, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to exec: " << e
			 << ": " << em << endl;
		return false;
	}
	return true;
}

bool RunTasks(sqlite3 *db)
{
	FutureTasks v;
	if (!CollectTasks(db, &v))
		return false;
	for (auto &f : v) {
		if (!f.get())
			return false;
	}
	return true;
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
