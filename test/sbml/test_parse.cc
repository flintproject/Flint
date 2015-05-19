/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "sbml.hh"

#include "database.h"

#define BOOST_TEST_MODULE test_parse
#include "test.hh"

struct F {
};

BOOST_FIXTURE_TEST_SUITE(test_parse, F)

BOOST_AUTO_TEST_CASE(BIOMD0000000114) {
	BOOST_CHECK_EQUAL(SaveGivenFile("BIOMD0000000114.db", TEST_MODELS("BIOMD0000000114.xml")), 1);
	BOOST_CHECK(flint::sbml::Parse("BIOMD0000000114.db"));
}

BOOST_AUTO_TEST_CASE(BIOMD0000000152) {
	BOOST_CHECK_EQUAL(SaveGivenFile("BIOMD0000000152.db", TEST_MODELS("BIOMD0000000152.xml")), 1);
	BOOST_CHECK(flint::sbml::Parse("BIOMD0000000152.db"));
}

BOOST_AUTO_TEST_SUITE_END()
