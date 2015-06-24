/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.hh"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include "database.h"
#include "db/driver.h"

#define BOOST_TEST_MODULE test_uot
#include "test.hh"

struct F {

	F()
		: driver_("") // generating a temporary file
	{}

	void Phml(const char *file) {
		SaveGivenFile(driver_.db(), file);
		BOOST_REQUIRE(phml::Read(driver_.db()));
	}

	void GenerateAndCompare(const char *output, const char *expected)
	{
		BOOST_REQUIRE(phml::UnitOfTime(driver_.db(), output));

		boost::filesystem::path fp(__FILE__);
		boost::filesystem::path ep = fp.parent_path();
		ep /= "uot";
		ep /= expected;
		boost::filesystem::ifstream ifs_o(output, std::ios::binary);
		boost::filesystem::ifstream ifs_e(ep, std::ios::binary);
		BOOST_REQUIRE(ifs_o);
		BOOST_REQUIRE(ifs_e);
		std::istream_iterator<char> b_o(ifs_o), e_o;
		std::istream_iterator<char> b_e(ifs_e), e_e;
		BOOST_CHECK_EQUAL_COLLECTIONS(b_o, e_o, b_e, e_e);

		boost::filesystem::remove(output);
	}

	db::Driver driver_;
};

BOOST_FIXTURE_TEST_SUITE(test_uot, F)

BOOST_AUTO_TEST_CASE(time_units) {
	Phml(TEST_MODELS("time_units.isml"));
	GenerateAndCompare("time_units.isml.uot",
					   "time_units.isml.bin");
}

BOOST_AUTO_TEST_SUITE_END()
