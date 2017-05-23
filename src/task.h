/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TASK_H_
#define FLINT_TASK_H_

#include "sqlite3.h"

#include <cstdio>
#include <memory>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

namespace flint {

struct Bytecode;
class Layout;

namespace filter {
class Writer;
}

namespace ls {
struct Configuration;
}

namespace task {

struct Task {
	std::unique_ptr<Bytecode> bc;
	std::unique_ptr<Bytecode> pre_bc;
	std::unique_ptr<Bytecode> post_bc;
	std::unique_ptr<Bytecode> reinit_bc;
	std::unique_ptr<Layout> layout;
	size_t granularity;
	double output_start_time;
	size_t layer_size;
	boost::interprocess::mapped_region control_mr;
	std::unique_ptr<filter::Writer> writer;
	std::unique_ptr<ls::Configuration> ls_config;
};

/*
 * Save a task's configuration into an attached database.
 * Return true in case of success, false otherwise.
 */
bool Config(int id, sqlite3 *db);

/*
 * List a task's spec.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Spec(int id, sqlite3 *db, FILE *fp);

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
