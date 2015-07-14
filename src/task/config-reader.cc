/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "task/config-reader.hh"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

using std::cerr;
using std::endl;
using std::memcpy;
using std::strcmp;

namespace task {

ConfigReader::ConfigReader(sqlite3 *db)
	: db::StatementDriver(db, "SELECT method, length, step, granularity FROM config")
	, method_(NULL)
	, length_(0)
	, step_(0)
	, granularity_(0)
{}

ConfigReader::~ConfigReader()
{
	delete [] method_;
}

bool ConfigReader::Read()
{
	int e;
	e = sqlite3_step(stmt());
	if (e != SQLITE_ROW) {
		cerr << "failed to step: " << e << endl;
		return false;
	}

	const unsigned char *method = sqlite3_column_text(stmt(), 0);
	if (!method) {
		cerr << "method is null" << endl;
		return false;
	}
	int len_m = sqlite3_column_bytes(stmt(), 0);
	method_ = new char[len_m+1];
	if (len_m > 0)
		memcpy(method_, method, len_m);
	method_[len_m] = '\0';

	length_ = sqlite3_column_double(stmt(), 1);
	if (length_ <= 0) {
		cerr << "length is <= 0" << endl;
		return false;
	}

	step_ = sqlite3_column_double(stmt(), 2);
	if (step_ <= 0) {
		cerr << "step is <= 0" << endl;
		return false;
	}

	granularity_ = sqlite3_column_int(stmt(), 3);
	if (granularity_ <= 0) {
		cerr << "granularity is non-positive" << endl;
		return false;
	}

	return true;
}

const char *ConfigReader::GetCanonicalMethodName()
{
	static const char kRk4[] = "rk4";
	if (!method_) return kRk4;
	if (strcmp(method_, "euler") == 0) return method_;
	return kRk4;
}

}
