/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.h"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include "database.h"

#define BOOST_TEST_MODULE test_nc
#include "test.h"

struct F : public test::MemoryFixture {

	void Phml(const char *file) {
		SaveGivenFile(driver_.db(), file);
		BOOST_REQUIRE(phml::Read(driver_.db()));
	}

	void GenerateAndCheck(const char *output, const char *expected)
	{
		BOOST_REQUIRE(phml::Nc(driver_.db(), output));
		BOOST_REQUIRE(boost::filesystem::is_regular_file(output));
		boost::filesystem::remove(output);

		test::Sql sql(driver_.db());
		std::vector<std::string> rows;
		rows.push_back(expected);
		sql.CheckRows("SELECT method FROM config", rows);
	}
};

BOOST_FIXTURE_TEST_SUITE(test_nc, F)

BOOST_AUTO_TEST_CASE(bogus_td) {
	Phml(TEST_MODELS("bogus-td.phml"));
	GenerateAndCheck("bogus-td.phml.nc",
					 "rk4");
}

BOOST_AUTO_TEST_CASE(Izhikevich_2003_model) {
	Phml(TEST_MODELS("Izhikevich_2003_model.isml"));
	GenerateAndCheck("Izhikevich_2003_model.isml.nc",
					 "rk4");
}

BOOST_AUTO_TEST_CASE(LR1_ring_reentry_initiate) {
	Phml(TEST_MODELS("LR1-ring_reentry_initiate.isml"));
	GenerateAndCheck("LR1-ring_reentry_initiate.isml.nc",
					 "4th-rungekutta");
}

BOOST_AUTO_TEST_CASE(Rybak_2006_with_static_instance_and_multiple_input) {
	Phml(TEST_MODELS("Rybak_2006_with_static_instance_and_multiple_input.isml"));
	GenerateAndCheck("Rybak_2006_with_static_instance_and_multiple_input.isml.nc",
					 "euler");
}

BOOST_AUTO_TEST_SUITE_END()
