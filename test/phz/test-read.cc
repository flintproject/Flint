/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phz.hh"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include "database.h"
#include "db/driver.hh"

#define BOOST_TEST_MODULE test_read
#include "test.hh"

struct F {

	F()
		: driver_(":memory:")
	{}

	void ReadAndCheck(const char *file) {
		BOOST_REQUIRE_EQUAL(SaveGivenFile(driver_.db(), file), 1);
		BOOST_CHECK(phz::Read(driver_.db(), "foo"));
		BOOST_CHECK(boost::filesystem::is_directory("foo"));
		BOOST_CHECK(boost::filesystem::is_regular_file("foo/model.phml"));
		boost::filesystem::remove_all("foo");
	}

	db::Driver driver_;
};

BOOST_FIXTURE_TEST_SUITE(test_read, F)

BOOST_AUTO_TEST_CASE(damping_oscillation) {
	ReadAndCheck(TEST_MODELS("damping_oscillation.phz"));
}

BOOST_AUTO_TEST_CASE(timeseries) {
	ReadAndCheck(TEST_MODELS("timeseries.phz"));
}

BOOST_AUTO_TEST_SUITE_END()
