/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.hh"

#include "database.h"

#define BOOST_TEST_MODULE test_uot
#include "test.hh"

struct F : public test::MemoryFixture {

	void Phml(const char *file) {
		SaveGivenFile(driver_.db(), file);
		BOOST_REQUIRE(phml::Read(driver_.db()));
	}

	void GenerateAndCompare(const char *output, const char *expected)
	{
		BOOST_REQUIRE(phml::UnitOfTime(driver_.db(), output));

		boost::filesystem::path op(output);
		boost::filesystem::path fp(__FILE__);
		boost::filesystem::path ep = fp.parent_path();
		ep /= "uot";
		ep /= expected;
		test::CheckSame(op, ep);
		boost::filesystem::remove(op);
	}
};

BOOST_FIXTURE_TEST_SUITE(test_uot, F)

BOOST_AUTO_TEST_CASE(time_units) {
	Phml(TEST_MODELS("time_units.isml"));
	GenerateAndCompare("time_units.isml.uot",
					   "time_units.isml.bin");
}

BOOST_AUTO_TEST_SUITE_END()
