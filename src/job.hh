/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_JOB_HH_
#define FLINT_JOB_HH_

#include <cstdio>
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
		   const char *source_layout_file, const char *source_data_file,
		   const char *target_layout_file, const char *target_data_file);

struct Option {
	size_t granularity;
	double output_start_time;
	const char *pre_file;
	const char *post_file;
	const char *filter_file;
	const char *input_data_file;
	const char *input_history_file;
	const char *control_file;
	const char *output_data_file;
	const char *output_history_file;
	void *progress_address;
};

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Evolve(sqlite3 *db,
			const char *layout_file,
			const char *bc_file,
			FILE *output_fp,
			const Option &option);

/*
 * Note that output_file is in the native encoding.
 * Return true in case of success, false otherwise.
 */
bool Job(const char *task_dir,
		 const char *job_dir,
		 void *progress_address,
		 const char *data_file,
		 const char *output_file,
		 const task::ConfigReader &reader,
		 sqlite3 *db);

}
}

#endif
