/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "sys/temporary_path.h"

#include <cstdio>
#include <fstream>
#include <sstream>
#define BOOST_TEST_MODULE test_temporary_path
#include <boost/test/unit_test.hpp>

struct F {
	F() : temporary_path_(new TemporaryPath) {}
	~F() {delete temporary_path_;}

	TemporaryPath *temporary_path_;
};

BOOST_FIXTURE_TEST_SUITE(test_temporary_path, F)

BOOST_AUTO_TEST_CASE(Touch) {
	char *p = temporary_path_->Touch();
	std::ifstream ifs(p);
	BOOST_CHECK(ifs.good());
	ifs.close();
	std::remove(p);
	free(p);
}

BOOST_AUTO_TEST_CASE(Create) {
	char *p = temporary_path_->Create("abc", 3);
	std::ifstream ifs(p);
	std::ostringstream oss;
	oss << ifs.rdbuf();
	BOOST_CHECK_EQUAL(oss.str(), "abc");
	ifs.close();
	std::remove(p);
	free(p);
}

BOOST_AUTO_TEST_SUITE_END()
