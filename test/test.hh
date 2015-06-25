/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEST_TEST_HH_
#define FLINT_TEST_TEST_HH_

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <boost/test/unit_test.hpp>

#include "sqlite3.h"

#define TEST_MODELS_0(dirname, basename) (#dirname "/" basename)
#define TEST_MODELS_1(dirname, basename) TEST_MODELS_0(dirname, basename)
#define TEST_MODELS(basename) TEST_MODELS_1(TEST_MODELS_DIR, basename)

namespace test {

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

	void Table(const char *table, std::vector<std::string> *rows) {
		std::ostringstream oss;
		oss << "SELECT * FROM " << table;
		std::string query = oss.str();
		int e = sqlite3_exec(db_, query.c_str(), AddRow, rows, NULL);
		BOOST_CHECK_EQUAL(e, SQLITE_OK);
	}

	void CheckTable(const char *table, std::vector<std::string> &expected) {
		std::vector<std::string> rows;
		Table(table, &rows);
		BOOST_CHECK_EQUAL_COLLECTIONS(rows.begin(), rows.end(),
									  expected.begin(), expected.end());
	}

private:
	sqlite3 *db_;
};

}

#endif
