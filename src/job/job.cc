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
#include "db/driver.h"
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

namespace job {

bool Job(int job_id, const char *data_file, const char *output_file,
		 const task::ConfigReader &reader, sqlite3 *db)
{
	boost::system::error_code ec;
	// ensure the job directory
	char dir[32];
	sprintf(dir, "%d", job_id);
	boost::filesystem::create_directory(dir, ec);
	if (ec) {
		cerr << "failed to create directory: " << ec << endl;
		return false;
	}

	char start_file[64];
	sprintf(start_file, "%d/start", job_id);
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

	char control_file[64];
	sprintf(control_file, "%d/control", job_id);
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

	char output_data_file[64];
	sprintf(output_data_file, "%d/output_data", job_id);
	char output_history_file[64];
	sprintf(output_history_file, "%d/output_history", job_id);
	char status_file[64];
	sprintf(status_file, "%d/status", job_id);

	job::Option option;
	if (boost::filesystem::exists("before-bc")) {
		option.pre_file = "before-bc";
	} else {
		option.pre_file = NULL;
	}
	if (boost::filesystem::exists("after-bc")) {
		option.post_file = "after-bc";
	} else {
		option.post_file = NULL;
	}
	option.filter_file = "filter";
	option.input_data_file = start_file;
	option.input_history_file = NULL;
	option.control_file = control_file;
	option.output_data_file = output_data_file;
	option.output_history_file = output_history_file;
	option.status_file = status_file;

	boost::filesystem::copy_file("isdh", output_file, ec);
	if (ec) {
		cerr << "failed to copy isdh to " << output_file
			 << ": " << ec << endl;
		return false;
	}
	FILE *ifp = fopen(start_file, "rb");
	if (!ifp) {
		perror(start_file);
		return false;
	}
	FILE *ofp = fopen(output_file, "ab");
	if (!ofp) {
		perror(output_file);
		fclose(ifp);
		return false;
	}
	if (!filter::Cut("filter", ifp, ofp)) {
		fclose(ofp);
		fclose(ifp);
		return false;
	}
	fclose(ifp);
	bool r = job::Evolve(db, "layout", "bc", ofp, option);
	fclose(ofp);
	return r;
}

}
