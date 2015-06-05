/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "file.hh"

#define BOOST_TEST_MODULE test_detect
#include "test.hh"

struct F {
	file::Format format;
};

BOOST_FIXTURE_TEST_SUITE(test_detect, F)

BOOST_AUTO_TEST_CASE(no_such_file) {
	BOOST_CHECK(!file::DetectFormat(TEST_MODELS("no_such_file"), &format));
}

BOOST_AUTO_TEST_CASE(BIOMD0000000114_xml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("BIOMD0000000114.xml"), &format));
	BOOST_CHECK_EQUAL(format, file::kSbml);
}

BOOST_AUTO_TEST_CASE(BIOMD0000000152_xml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("BIOMD0000000152.xml"), &format));
	BOOST_CHECK_EQUAL(format, file::kSbml);
}

BOOST_AUTO_TEST_CASE(damping_oscillation_phz) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("damping_oscillation.phz"), &format));
	BOOST_CHECK_EQUAL(format, file::kPhz);
}

BOOST_AUTO_TEST_CASE(empty_phml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("empty.phml"), &format));
	BOOST_CHECK_EQUAL(format, file::kIsml);
}

BOOST_AUTO_TEST_CASE(Izhikevich_2003_phsp) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("Izhikevich_2003.phsp"), &format));
	BOOST_CHECK_EQUAL(format, file::kPhsp);
}

BOOST_AUTO_TEST_CASE(Izhikevich_2003_xml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("Izhikevich_2003.xml"), &format));
	BOOST_CHECK_EQUAL(format, file::kSedml);
}

BOOST_AUTO_TEST_CASE(lockwood_ewy_hermann_holford_2006_cellml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("lockwood_ewy_hermann_holford_2006.cellml"), &format));
	BOOST_CHECK_EQUAL(format, file::kCellml);
}

BOOST_AUTO_TEST_CASE(LR_pulse_189_164_isml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("LR-pulse-189-164.isml"), &format));
	BOOST_CHECK_EQUAL(format, file::kIsml);
}

BOOST_AUTO_TEST_CASE(mathml_cosh_xml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("mathml_cosh.xml"), &format));
	BOOST_CHECK_EQUAL(format, file::kMathml);
}

BOOST_AUTO_TEST_CASE(yang_tong_mccarver_hines_beard_2006_cellml) {
	BOOST_CHECK(file::DetectFormat(TEST_MODELS("yang_tong_mccarver_hines_beard_2006.cellml"), &format));
	BOOST_CHECK_EQUAL(format, file::kCellml);
}

BOOST_AUTO_TEST_SUITE_END()
