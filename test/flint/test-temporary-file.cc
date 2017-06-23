/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/temporary-file.h"

#define BOOST_TEST_MODULE test_temporary_file
#include "test.h"

struct F {
	flint::TemporaryFile temp_file;
};

BOOST_FIXTURE_TEST_SUITE(test_temporary_file, F)

BOOST_AUTO_TEST_CASE(basics)
{
	BOOST_CHECK(temp_file.ofs());
	temp_file.Close();
	BOOST_CHECK(boost::filesystem::is_regular_file(temp_file.path()));
}

BOOST_AUTO_TEST_CASE(rename)
{
	boost::filesystem::remove("test-temporary-file.out");
	temp_file.Rename("test-temporary-file.out");
	BOOST_CHECK(!boost::filesystem::is_regular_file(temp_file.path()));
	BOOST_CHECK(boost::filesystem::is_regular_file("test-temporary-file.out"));
}

BOOST_AUTO_TEST_SUITE_END()
