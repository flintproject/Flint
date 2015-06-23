/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "sedml.hh"

#include "db/driver.h"

#define BOOST_TEST_MODULE test_read
#include "test.hh"

struct F {

	F()
		: driver_("") // generating a temporary file
	{
	}

	db::Driver driver_;
};

BOOST_FIXTURE_TEST_SUITE(test_read, F)

BOOST_AUTO_TEST_CASE(Izhikevich_2003) {
	BOOST_CHECK(sedml::Read(TEST_MODELS("Izhikevich_2003.xml"), driver_.db()));

	test::Sql sql(driver_.db());

	std::vector<std::string> tasks;
	sql.Table("tasks", &tasks);
	BOOST_CHECK_EQUAL(tasks.size(), 1u);
	BOOST_CHECK_EQUAL(tasks[0], "1 1");

	std::vector<std::string> models;
	sql.Table("models", &models);
	BOOST_CHECK_EQUAL(models.size(), 1u);

	std::vector<std::string> sims;
	sql.Table("sims", &sims);
	BOOST_CHECK_EQUAL(sims.size(), 1u);
	BOOST_CHECK_EQUAL(sims[0], "euler 300.0 0.01 1");

	std::vector<std::string> dgs;
	sql.Table("dgs", &dgs);
	BOOST_CHECK_EQUAL(dgs.size(), 6u);
	BOOST_CHECK_EQUAL(dgs[0], "1 e92c7a90-6bf0-4c35-817d-fd9505050830:v");
	BOOST_CHECK_EQUAL(dgs[1], "1 e92c7a90-6bf0-4c35-817d-fd9505050830:u");
	BOOST_CHECK_EQUAL(dgs[2], "1 e92c7a90-6bf0-4c35-817d-fd9505050830:Iext");
	BOOST_CHECK_EQUAL(dgs[3], "1 055edf9e-8eb2-4812-abb2-e3e6f5be12e1:u");
	BOOST_CHECK_EQUAL(dgs[4], "1 055edf9e-8eb2-4812-abb2-e3e6f5be12e1:v");
	BOOST_CHECK_EQUAL(dgs[5], "1 917c40a3-2881-4b6e-8d5c-3fff0145a898:stimulus_current");
}

BOOST_AUTO_TEST_SUITE_END()
