/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEST_TEST_HH_
#define FLINT_TEST_TEST_HH_

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <boost/test/unit_test.hpp>

#include "db/driver.hh"
#include "sqlite3.h"

#define TEST_MODELS_0(dirname, basename) (#dirname "/" basename)
#define TEST_MODELS_1(dirname, basename) TEST_MODELS_0(dirname, basename)
#define TEST_MODELS(basename) TEST_MODELS_1(TEST_MODELS_DIR, basename)

namespace test {

struct MemoryFixture {

	MemoryFixture()
		: driver_(":memory:") // In-memory database
	{}

	db::Driver driver_;
};

namespace {

int AddRow(void *data, int argc, char **argv, char **names)
{
	std::vector<std::string> *rows = static_cast<std::vector<std::string> *>(data);
	(void)names;
	std::ostringstream oss;
	for (int i=0;i<argc;i++) {
		if (i != 0) oss.put(' ');
		oss << argv[i];
	}
	rows->push_back(oss.str());
	return 0;
}

}

class Sql {
public:
	explicit Sql(sqlite3 *db)
		: db_(db)
	{}

	void Exec(const char *query) {
		int e = sqlite3_exec(db_, query, NULL, NULL, NULL);
		BOOST_CHECK_EQUAL(e, SQLITE_OK);
	}

	void CheckRows(const char *query, const std::vector<std::string> &expected) {
		std::vector<std::string> rows;
		CollectRows(query, &rows);
		BOOST_CHECK_EQUAL_COLLECTIONS(rows.cbegin(), rows.cend(),
									  expected.cbegin(), expected.cend());
	}

	void Table(const char *table, std::vector<std::string> *rows)
	{
		std::ostringstream oss;
		oss << "SELECT * FROM " << table;
		std::string query = oss.str();
		CollectRows(query.c_str(), rows);
	}

	void CheckTable(const char *table, const std::vector<std::string> &expected) {
		std::vector<std::string> rows;
		Table(table, &rows);
		BOOST_CHECK_EQUAL_COLLECTIONS(rows.begin(), rows.end(),
									  expected.begin(), expected.end());
	}

private:
	void CollectRows(const char *query, std::vector<std::string> *rows)
	{
		int e = sqlite3_exec(db_, query, &AddRow, rows, NULL);
		BOOST_CHECK_EQUAL(e, SQLITE_OK);
	}

	sqlite3 *db_;
};

class StderrCapture {
public:
	StderrCapture()
		: oss_()
		, orig_(std::cerr.rdbuf(oss_.rdbuf()))
	{}

	~StderrCapture() {
		std::cerr.rdbuf(orig_);
	}

	std::string Get() {
		return oss_.str();
	}

private:
	std::ostringstream oss_;
	std::streambuf *orig_;
};

}

#endif
