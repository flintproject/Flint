/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "bc/binary.h"
#include "compiler.h"
#include "database.h"
#include "db/driver.h"
#include "db/read-only-driver.h"
#include "exec.h"
#include "filter.h"
#include "job.h"
#include "phsp.h"
#include "sedml.h"
#include "solver.h"
#include "task.h"
#include "task/config-reader.h"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::perror;
using std::strcmp;

namespace flint {
namespace job {

bool Job(const char *task_dir,
		 const char *job_dir,
		 void *progress_address,
		 const char *data_file,
		 const char *output_file,
		 const task::ConfigReader &reader,
		 sqlite3 *db)
{
	static const int kShort = 64;
	static const int kLong = 96;

	boost::system::error_code ec;
	// ensure the job directory
	boost::filesystem::create_directories(job_dir, ec);
	if (ec) {
		cerr << "failed to create directory: " << ec << endl;
		return false;
	}

	char start_file[kLong];
	sprintf(start_file, "%s/start", job_dir);
	boost::filesystem::copy_file(data_file, start_file, ec);
	if (ec) {
		cerr << "failed to copy file: " << ec << endl;
		return false;
	}
	FILE *fp = fopen(start_file, "r+b");
	if (!fp) {
		perror(start_file);
		return false;
	}
	if (!task::Timer(reader.length(), reader.step(), fp))
		return false;
	fclose(fp);

	char control_file[kLong];
	sprintf(control_file, "%s/control", job_dir);
	fp = fopen(control_file, "w");
	if (!fp) {
		perror(control_file);
		return false;
	}
	if (fputc('0', fp) == EOF) {
		fclose(fp);
		return false;
	}
	fclose(fp);

	char output_data_file[kLong];
	sprintf(output_data_file, "%s/output-data", job_dir);
	char output_history_file[kLong];
	sprintf(output_history_file, "%s/output-history", job_dir);

	job::Option option;
	{
		task::ConfigReader reader(db);
		if (!reader.Read())
			return false;
		option.granularity = reader.granularity();
		option.output_start_time = reader.output_start_time();
	}
	option.task_dir = task_dir;
	char before_bc_file[kShort];
	sprintf(before_bc_file, "%s/before-bc", task_dir);
	if (boost::filesystem::exists(before_bc_file)) {
		option.pre_file = before_bc_file;
	} else {
		option.pre_file = nullptr;
	}
	char after_bc_file[kShort];
	sprintf(after_bc_file, "%s/after-bc", task_dir);
	if (boost::filesystem::exists(after_bc_file)) {
		option.post_file = after_bc_file;
	} else {
		option.post_file = nullptr;
	}
	char filter_file[kShort];
	sprintf(filter_file, "%s/filter", task_dir);
	option.filter_file = filter_file;
	option.input_data_file = start_file;
	option.input_history_file = nullptr;
	option.control_file = control_file;
	option.output_data_file = output_data_file;
	option.output_history_file = output_history_file;
	option.progress_address = progress_address;

	char isdh_file[kShort];
	sprintf(isdh_file, "%s/isdh", task_dir);
	boost::filesystem::copy_file(isdh_file, output_file, ec);
	if (ec) {
		cerr << "failed to copy "
			 << isdh_file
			 << " to "
			 << output_file
			 << ": " << ec << endl;
		return false;
	}
	FILE *ofp = fopen(output_file, "ab");
	if (!ofp) {
		perror(output_file);
		return false;
	}
	// write initial values only when output_start_time is 0.
	if (option.output_start_time == 0) {
		FILE *ifp = fopen(start_file, "rb");
		if (!ifp) {
			perror(start_file);
			fclose(ofp);
			return false;
		}
		if (!filter::Cut(filter_file, ifp, ofp)) {
			fclose(ofp);
			fclose(ifp);
			return false;
		}
		fclose(ifp);
	}
	option.output_fp = ofp;

	char stats_file[kLong];
	std::sprintf(stats_file, "%s/stats", job_dir);
	FILE *sfp = std::fopen(stats_file, "w");
	if (!sfp) {
		std::fclose(ofp);
		std::perror(stats_file);
		return false;
	}
	option.stats_fp = sfp;

	char layout_file[kShort];
	sprintf(layout_file, "%s/layout", task_dir);
	option.layout_file = layout_file;

	bool r;
	if (reader.GetMethod() == compiler::Method::kArk) {
		r = solver::Solve(db, solver::Method::kArk, option);
	} else {
		char bc_file[kShort];
		sprintf(bc_file, "%s/bc", task_dir);
		r = job::Evolve(db, bc_file, option);
	}
	std::fclose(sfp);
	fclose(ofp);
	return r;
}

}
}
