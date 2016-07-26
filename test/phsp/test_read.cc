/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phsp.h"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include "sedml.h"

#define BOOST_TEST_MODULE test_read
#include "test.h"

BOOST_FIXTURE_TEST_SUITE(test_read, test::MemoryFixture)

BOOST_AUTO_TEST_CASE(Izhikevich_2003) {
	BOOST_CHECK(sedml::Read(TEST_MODELS("Izhikevich_2003.xml"), driver_.db()));
	BOOST_CHECK(phsp::Read(TEST_MODELS("Izhikevich_2003.phsp"), driver_.db()));
	BOOST_CHECK(boost::filesystem::is_directory("1"));
	BOOST_CHECK(boost::filesystem::is_regular_file("1/task.db"));
	BOOST_CHECK_EQUAL(boost::filesystem::remove_all("1"), 2u);
}

BOOST_AUTO_TEST_SUITE_END()
