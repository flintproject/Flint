/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_JOB_H_
#define FLINT_JOB_H_

#include <cstdio>
#include <vector>
#include "sqlite3.h"

namespace flint {
namespace task {
class ConfigReader;
}

namespace job {

/*
 * Return newly-allocated char[] the client code should be responsible
 * for freeing.
 */
char *BuildPath(const char *dir, int id);

/*
 * Return true in case of success, false otherwise.
 */
bool Generate(sqlite3 *input, const char *dir, int *job_id);

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Store(sqlite3 *db,
		   const char *source_layout_file, double *source_data,
		   const char *target_layout_file, double *target_data);

struct Option {
	size_t granularity;
	double output_start_time;
	const char *task_dir;
	const char *pre_file;
	const char *post_file;
	const char *layout_file;
	const char *filter_file;
	std::vector<double> *input_data;
	const char *input_history_file;
	const char *control_file;
	const char *output_data_file;
	const char *output_history_file;
	FILE *output_fp;
	FILE *stats_fp;
	void *progress_address;
};

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Evolve(sqlite3 *db,
			const char *bc_file,
			const Option &option);

/*
 * Note that output_file is in the native encoding.
 * Return true in case of success, false otherwise.
 */
bool Job(const char *task_dir,
		 const char *job_dir,
		 void *progress_address,
		 std::vector<double> *data,
		 const char *output_file,
		 const task::ConfigReader &reader,
		 sqlite3 *db);

}
}

#endif
