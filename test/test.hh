/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEST_TEST_HH_
#define FLINT_TEST_TEST_HH_

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>

#include "db/driver.hh"
#include "sqlite3.h"

using namespace flint;

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
		if (argv[i]) { // print non-null only
			oss << argv[i];
		}
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

struct TemporaryWorkingDirectory {

	TemporaryWorkingDirectory()
		: original_path_(boost::filesystem::current_path())
	{}

	~TemporaryWorkingDirectory() {
		boost::filesystem::current_path(original_path_);
	}

	void PushWorkingDirectory(const char *name) {
		boost::filesystem::path p(original_path_);
		p /= name;
		if (boost::filesystem::exists(p))
			boost::filesystem::remove_all(p);
		BOOST_CHECK(boost::filesystem::create_directory(p));
		boost::filesystem::current_path(p);
		current_path_ = p;
	}

	void PopWorkingDirectory() {
		boost::filesystem::current_path(original_path_);
		boost::filesystem::remove_all(current_path_);
	}

	boost::filesystem::path original_path_;
	boost::filesystem::path current_path_;
};

inline void CheckSame(boost::filesystem::path p0,
					  boost::filesystem::path p1)
{
	boost::filesystem::ifstream ifs0(p0, std::ios::binary);
	boost::filesystem::ifstream ifs1(p1, std::ios::binary);
	BOOST_REQUIRE(ifs0);
	BOOST_REQUIRE(ifs1);
	ifs0.unsetf(std::ios::skipws);
	ifs1.unsetf(std::ios::skipws);
	std::istream_iterator<char> b0(ifs0), e0;
	std::istream_iterator<char> b1(ifs1), e1;
	BOOST_CHECK_EQUAL_COLLECTIONS(b0, e0, b1, e1);
	ifs0.close();
	ifs1.close();
}

inline void CheckDifference(boost::filesystem::path p0,
							boost::filesystem::path p1)
{
	boost::filesystem::ifstream ifs0(p0, std::ios::binary);
	boost::filesystem::ifstream ifs1(p1, std::ios::binary);
	BOOST_REQUIRE(ifs0);
	BOOST_REQUIRE(ifs1);
	ifs0.unsetf(std::ios::skipws);
	ifs1.unsetf(std::ios::skipws);
	std::istream_iterator<char> b0(ifs0), e0;
	std::istream_iterator<char> b1(ifs1), e1;
	std::istream_iterator<char> it0, it1;
	bool different = false;
	for (it0=b0,it1=b1;it0!=e0&&it1!=e1;++it0,++it1) {
		if (*it0 != *it1) {
			different = true;
			break;
		}
	}
	if (it0 != e0 || it1 != e1) {
		different = true;
	}
	BOOST_CHECK(different);
	ifs0.close();
	ifs1.close();
}

}

#endif
