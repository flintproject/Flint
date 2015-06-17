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
using std::sprintf;

namespace exec {

namespace {

bool Task(const char *dir)
{
	static const int kShort = 64;
	static const int kLong = 96;

	char canceled_file[kShort];
	sprintf(canceled_file, "%s/canceled", dir);
	if (boost::filesystem::exists(canceled_file)) {
		// exit early if file "canceled" exists
		return true;
	}

	char modeldb_file[kShort];
	sprintf(modeldb_file, "%s/modeldb", dir);
	db::ReadOnlyDriver m(modeldb_file);
	char spec_file[kShort];
	sprintf(spec_file, "%s/spec.txt", dir);
	char layout_file[kShort];
	sprintf(layout_file, "%s/layout", dir);
	char filter_file[kShort];
	sprintf(filter_file, "%s/filter", dir);
	if (!filter::Create(m.db(), spec_file, layout_file, filter_file))
		return false;
	char track_file[kShort];
	sprintf(track_file, "%s/track", dir);
	if (!filter::Track(filter_file, track_file))
		return false;
	char isdh_file[kShort];
	sprintf(isdh_file, "%s/isdh", dir);
	if (!filter::Isdh(filter_file, isdh_file))
		return false;
	task::ConfigReader reader(m.db());
	if (!reader.Read())
		return false;
	char bc_file[kShort];
	sprintf(bc_file, "%s/bc", dir);
	if (!compiler::Compile(m.db(), "input_eqs", reader.GetCanonicalMethodName(), bc_file))
		return false;

	char db_file[kShort];
	sprintf(db_file, "%s/db", dir);
	db::Driver driver(db_file);
	if (!exec::Enum(driver.db()))
		return false;
	if (!task::Form(driver.db()))
		return false;
	char generated_layout_file[kShort];
	sprintf(generated_layout_file, "%s/generated-layout", dir);
	if (!layout::Generate(driver.db(), generated_layout_file))
		return false;
	int job_id;
	for (;;) {
		if (!job::Generate(driver.db(), dir, &job_id))
			return false;
		if (job_id == 0)
			break; // all jobs are done now

		char job_dir[kShort];
		sprintf(job_dir, "%s/%d", dir, job_id);

		char db_file[kLong];
		sprintf(db_file, "%s/generated.db", job_dir);
		db::ReadOnlyDriver g(db_file);

		char generated_bc_file[kLong];
		sprintf(generated_bc_file, "%s/generated-bc", job_dir);
		if (!compiler::Compile(g.db(), "parameter_eqs", "assign", generated_bc_file))
			return false;

		char generated_init_file[kLong];
		sprintf(generated_init_file, "%s/generated-init",job_dir);
		if (!runtime::Init(driver.db(), generated_layout_file, generated_bc_file, generated_init_file))
			return false;

		char init_file[kShort];
		sprintf(init_file, "%s/init", dir);
		char stored_file[kLong];
		sprintf(stored_file, "%s/stored", job_dir);
		boost::system::error_code ec;
		boost::filesystem::copy_file(init_file, stored_file, ec);
		if (ec) {
			cerr << "failed to copy init to " << stored_file
				 << ": " << ec << endl;
			return false;
		}
		if (!job::Store(driver.db(), generated_layout_file, generated_init_file, layout_file, stored_file))
			return false;

		char isd_file[kLong];
		sprintf(isd_file, "%s/isd", job_dir);
		if (!job::Job(dir, job_dir, stored_file, isd_file, reader, m.db()))
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
