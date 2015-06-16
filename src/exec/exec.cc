/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "compiler.hh"
#include "db/driver.h"
#include "db/read-only-driver.hh"
#include "filter.hh"
#include "job.hh"
#include "layout.hh"
#include "phsp.hh"
#include "runtime.hh"
#include "sedml.hh"
#include "task.hh"
#include "task/config-reader.hh"

using std::cerr;
using std::endl;

namespace exec {

namespace {

bool Task(const char *dir)
{
	boost::system::error_code ec;
	boost::filesystem::current_path(dir, ec);
	if (ec) {
		cerr << "failed to change directory to " << dir
			 << ": " << ec << endl;
		return false;
	}

	if (boost::filesystem::exists("canceled")) {
		// exit early if file "canceled" exists
		return true;
	}

	db::ReadOnlyDriver m("modeldb");
	if (!filter::Create(m.db(), "spec.txt", "layout", "filter"))
		return false;
	if (!filter::Track("filter", "track"))
		return false;
	if (!filter::Isdh("filter", "isdh"))
		return false;
	task::ConfigReader reader(m.db());
	if (!reader.Read())
		return false;
	if (!compiler::Compile(m.db(), "input_eqs", reader.GetCanonicalMethodName(), "bc"))
		return false;

	db::Driver driver("db");
	if (!exec::Enum(driver.db()))
		return false;
	if (!task::Form(driver.db()))
		return false;
	if (!layout::Generate(driver.db(), "generated-layout"))
		return false;
	int job_id;
	for (;;) {
		if (!job::Generate(driver.db(), &job_id))
			return false;
		if (job_id == 0)
			break; // all jobs are done now

		char db_file[64];
		sprintf(db_file, "%d/generated.db", job_id);

		db::ReadOnlyDriver g(db_file);

		char bc_file[64];
		sprintf(bc_file, "%d/generated-bc", job_id);
		if (!compiler::Compile(g.db(), "parameter_eqs", "assign", bc_file))
			return false;

		char init_file[64];
		sprintf(init_file, "%d/generated-init", job_id);
		if (!runtime::Init(driver.db(), "generated-layout", bc_file, init_file))
			return false;

		char stored_file[64];
		sprintf(stored_file, "%d/stored", job_id);
		boost::filesystem::copy_file("init", stored_file, ec);
		if (ec) {
			cerr << "failed to copy init to " << stored_file
				 << ": " << ec << endl;
			return false;
		}
		if (!job::Store(driver.db(), "generated-layout", init_file, "layout", stored_file))
			return false;

		char isd_file[64];
		sprintf(isd_file, "%d/isd", job_id);
		if (!job::Job(job_id, stored_file, isd_file, reader, m.db()))
			return false;
	}

	boost::filesystem::current_path("..", ec);
	if (ec) {
		cerr << "failed to change directory to ..:" << ec << endl;
		return false;
	}
	return true;
}

int CallTask(void *data, int argc, char **argv, char **names)
{
	(void)data;
	(void)names;
	assert(argc == 1);
	return Task(argv[0]) ? 0 : 1;
}

}

bool Exec(sqlite3 *db)
{
	if (!sedml::Read(db))
		return false;
	if (!phsp::Read(db))
		return false;

	char *em;
	int e;
	e = sqlite3_exec(db,
					 "SELECT t.rowid FROM tasks AS t WHERE EXISTS (SELECT * FROM models AS m WHERE t.model_id = m.rowid)",
					 &CallTask, NULL, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to exec: " << e
			 << ": " << em << endl;
		return false;
	}
	return true;
}

}
