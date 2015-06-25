/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "layout.hh"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include "database.h"
#include "db/driver.hh"
#include "phml.hh"

#define BOOST_TEST_MODULE test_generate
#include "test.hh"

struct F {

	F()
		: driver_(":memory:")
	{}

	void GenerateAndCheck(const char *file) {
		BOOST_REQUIRE_EQUAL(SaveGivenFile(driver_.db(), file), 1);
		BOOST_REQUIRE(phml::Read(driver_.db()));
		BOOST_CHECK(layout::Generate(driver_.db(), "layout"));
		BOOST_CHECK(boost::filesystem::is_regular_file("layout"));
		boost::filesystem::remove_all("layout");
	}

	db::Driver driver_;
};

BOOST_FIXTURE_TEST_SUITE(test_generate, F)

BOOST_AUTO_TEST_CASE(Rybak_2006_with_static_instance_and_multiple_input) {
	GenerateAndCheck(TEST_MODELS("Rybak_2006_with_static_instance_and_multiple_input.isml"));
}

BOOST_AUTO_TEST_CASE(ringed_Beeler_Reuter_1977_model_with_static_instance) {
	GenerateAndCheck(TEST_MODELS("ringed_Beeler_Reuter_1977_model_with_static_instance.isml"));
}

BOOST_AUTO_TEST_SUITE_END()
