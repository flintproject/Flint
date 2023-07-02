/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "task/config-reader.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

namespace flint {
namespace task {

ConfigReader::ConfigReader(sqlite3 *db)
	: db::StatementDriver(db, "SELECT method, length, step, granularity, output_start_time, dps_path FROM config")
	, length_(0)
	, step_(0)
	, granularity_(0)
	, output_start_time_(0)
{}

ConfigReader::~ConfigReader()
{
}

bool ConfigReader::Read()
{
	int e;
	e = sqlite3_step(stmt());
	if (e != SQLITE_ROW) {
		std::cerr << "failed to step: " << e << std::endl;
		return false;
	}

	const unsigned char *method = sqlite3_column_text(stmt(), 0);
	if (!method) {
		std::cerr << "method is null" << std::endl;
		return false;
	}
	int len_m = sqlite3_column_bytes(stmt(), 0);
	method_.reset(new char[len_m+1]);
	if (len_m > 0)
		std::memcpy(method_.get(), method, len_m);
	method_[len_m] = '\0';

	length_ = sqlite3_column_double(stmt(), 1);
	if (length_ <= 0) {
		std::cerr << "length is <= 0" << std::endl;
		return false;
	}

	step_ = sqlite3_column_double(stmt(), 2);
	if (step_ <= 0) {
		std::cerr << "step is <= 0" << std::endl;
		return false;
	}

	granularity_ = sqlite3_column_int(stmt(), 3);
	if (granularity_ <= 0) {
		std::cerr << "granularity is non-positive" << std::endl;
		return false;
	}

	output_start_time_ = sqlite3_column_double(stmt(), 4);
	if (output_start_time_ < 0) {
		std::cerr << "output_start_time is negative: "
			 << output_start_time_
			 << std::endl;
		return false;
	}

	const unsigned char *dps_path = sqlite3_column_text(stmt(), 5);
	if (!dps_path)
		return true; // NULL is OK for dps_path
	int len_d = sqlite3_column_bytes(stmt(), 5);
	if (len_d == 0)
		return true; // empty dps_path is the same as NULL
	dps_path_.reset(new char[len_d+1]);
	std::memcpy(dps_path_.get(), dps_path, len_d);
	dps_path_[len_d] = '\0';

	return true;
}

compiler::Method ConfigReader::GetMethod() const
{
	if (!method_)
		return compiler::Method::kRk4;
	if (std::strcmp(method_.get(), "euler") == 0)
		return compiler::Method::kEuler;
	if (std::strcmp(method_.get(), "ark") == 0)
		return compiler::Method::kArk;
	if (std::strcmp(method_.get(), "euler-maruyama") == 0)
		return compiler::Method::kEulerMaruyama;
	return compiler::Method::kRk4;
}

const char *ConfigReader::GetDpsPath() const
{
	return dps_path_.get();
}

}
}
