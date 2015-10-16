/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "bc/binary.h"
#include "compiler.hh"
#include "database.h"
#include "db/driver.hh"
#include "db/read-only-driver.hh"
#include "exec.hh"
#include "filter.hh"
#include "job.hh"
#include "phsp.hh"
#include "sedml.hh"
#include "task.hh"
#include "task/config-reader.hh"

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
	boost::filesystem::create_directory(job_dir, ec);
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
	char status_file[kLong];
	sprintf(status_file, "%s/status", job_dir);

	job::Option option;
	{
		task::ConfigReader reader(db);
		if (!reader.Read())
			return false;
		option.granularity = reader.granularity();
		option.output_start_time = reader.output_start_time();
	}
	char before_bc_file[kShort];
	sprintf(before_bc_file, "%s/before-bc", task_dir);
	if (boost::filesystem::exists(before_bc_file)) {
		option.pre_file = before_bc_file;
	} else {
		option.pre_file = NULL;
	}
	char after_bc_file[kShort];
	sprintf(after_bc_file, "%s/after-bc", task_dir);
	if (boost::filesystem::exists(after_bc_file)) {
		option.post_file = after_bc_file;
	} else {
		option.post_file = NULL;
	}
	char filter_file[kShort];
	sprintf(filter_file, "%s/filter", task_dir);
	option.filter_file = filter_file;
	option.input_data_file = start_file;
	option.input_history_file = NULL;
	option.control_file = control_file;
	option.output_data_file = output_data_file;
	option.output_history_file = output_history_file;
	option.status_file = status_file;
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
	char layout_file[kShort];
	sprintf(layout_file, "%s/layout", task_dir);
	char bc_file[kShort];
	sprintf(bc_file, "%s/bc", task_dir);
	bool r = job::Evolve(db, layout_file, bc_file, ofp, option);
	fclose(ofp);
	return r;
}

}
}
