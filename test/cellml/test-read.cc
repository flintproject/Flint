/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "cellml.h"

#include "database.h"

#define BOOST_TEST_MODULE test_read
#include "test.h"

BOOST_FIXTURE_TEST_SUITE(test_read, test::MemoryFixture)

BOOST_AUTO_TEST_CASE(lockwood_ewy_hermann_holford_2006) {
	BOOST_REQUIRE_EQUAL(SaveGivenFile(driver_.db(), TEST_MODELS("lockwood_ewy_hermann_holford_2006.cellml")), 1);
	BOOST_CHECK(cellml::Read(driver_.db()));
}

BOOST_AUTO_TEST_CASE(yang_tong_mccarver_hines_beard_2006) {
	BOOST_REQUIRE_EQUAL(SaveGivenFile(driver_.db(), TEST_MODELS("yang_tong_mccarver_hines_beard_2006.cellml")), 1);
	BOOST_CHECK(cellml::Read(driver_.db()));
}

BOOST_AUTO_TEST_SUITE_END()
