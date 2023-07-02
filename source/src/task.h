/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TASK_H_
#define FLINT_TASK_H_

#include "sqlite3.h"

#include <iostream>
#include <memory>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

#include "compiler.h"
#include "runtime/flow.h"
#include "runtime/timeseries.h"

namespace flint {

struct Bytecode;
class Layout;

namespace cas {
class System;
}

namespace filter {
class Writer;
}

namespace ls {
struct Configuration;
}

namespace task {

struct Task {
	FlowInboundMap inbound;
	TimeseriesVector tv;
	std::unique_ptr<Bytecode> bc;
	std::unique_ptr<Bytecode> pre_bc;
	std::unique_ptr<Bytecode> post_bc;
	std::unique_ptr<Bytecode> reinit_bc;
	std::unique_ptr<cas::System> system;
	compiler::Method method;
	double length;
	double step;
	size_t granularity;
	double output_start_time;
	std::unique_ptr<Layout> layout;
	size_t layer_size;
	boost::interprocess::mapped_region control_mr;
	boost::interprocess::mapped_region progress_mr;
	boost::interprocess::mapped_region rss_mr;
	std::unique_ptr<filter::Writer> writer;
	std::unique_ptr<ls::Configuration> ls_config;

	bool IsCanceled() const;

	char *GetControlAddress(int job_id) const;

	char *GetProgressAddress(int job_id) const;

	/*
	 * Return nullptr unless the method of least-squares is used.
	 */
	double *GetRssAddress(int job_id) const;
};

/*
 * Save a task's configuration into an attached database.
 * Return true in case of success, false otherwise.
 */
bool Config(int id, sqlite3 *db, const boost::filesystem::path &dir);

/*
 * List a task's spec.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Spec(int id, sqlite3 *db, std::ostream &os);

/*
 * Return true in case of success, false otherwise.
 */
bool Form(sqlite3 *db);

/*
 * Return true in case of success, false otherwise.
 */
bool Timer(double length, double step, double *data);

}
}

#endif
