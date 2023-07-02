/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_JOB_H_
#define FLINT_JOB_H_

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

namespace flint {

struct Bytecode;

namespace ctrl {
struct Argument;
}

namespace task {
class ConfigReader;
struct Task;
}

namespace fppp {
struct Option;
}

namespace job {

/*
 * Return path of job `id'.
 */
boost::filesystem::path BuildPath(const boost::filesystem::path &dir, int id);

/*
 * Return true in case of success, false otherwise.
 */
bool Generate(sqlite3 *input, const boost::filesystem::path &dir, int *job_id);

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Store(sqlite3 *db,
		   const boost::filesystem::path &source_layout_file, double *source_data,
		   const boost::filesystem::path &target_layout_file, double *target_data);

struct Option {
	int id;
	boost::filesystem::path dir;
	std::vector<double> *input_data;
	boost::filesystem::path input_history_file;
	boost::filesystem::path output_data_file;
	boost::filesystem::path output_history_file;
	std::ostream *output_stream;
	ctrl::Argument *arg;
	const fppp::Option *fppp_option;
};

enum class Result {
	kSucceeded,
	kCancelled,
	kFailed
};

/*
 * Note that db is for read only.
 */
Result Evolve(task::Task &task,
			const Option &option);

/*
 * Note that output_file is in the native encoding.
 */
Result Job(int id,
		 ctrl::Argument *arg,
		 const boost::filesystem::path &task_dir,
		 const boost::filesystem::path &job_dir,
		 task::Task &task,
		 const fppp::Option *fppp_option,
		 std::vector<double> *data,
		 const boost::filesystem::path &output_file);

}
}

#endif
