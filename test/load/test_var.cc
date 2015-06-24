/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load/var.hh"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include "cellml.hh"
#include "database.h"
#include "db/driver.hh"
#include "phml.hh"
#include "sbml.hh"

#define BOOST_TEST_MODULE test_var
#include "test.hh"

struct F {

	F()
		: driver_("") // generating a temporary file
	{}

	void Cellml(const char *file) {
		SaveGivenFile(driver_.db(), file);
		BOOST_REQUIRE(cellml::Read(driver_.db()));
	}

	void Phml(const char *file) {
		SaveGivenFile(driver_.db(), file);
		BOOST_REQUIRE(phml::Read(driver_.db()));
	}

	void Sbml(const char *file) {
		SaveGivenFile(driver_.db(), file);
		BOOST_REQUIRE(flint::sbml::Read(driver_.db()));
	}

	void GenerateAndCompare(const char *output, const char *expected)
	{
		BOOST_REQUIRE(load::Var(driver_.db(), output));

		boost::filesystem::path fp(__FILE__);
		boost::filesystem::path ep = fp.parent_path();
		ep /= "var";
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

BOOST_FIXTURE_TEST_SUITE(test_var, F)

BOOST_AUTO_TEST_CASE(lockwood_ewy_hermann_holford_2006) {
	Cellml(TEST_MODELS("lockwood_ewy_hermann_holford_2006.cellml"));
	GenerateAndCompare("lockwood_ewy_hermann_holford_2006.cellml.var",
					   "lockwood_ewy_hermann_holford_2006.cellml.bin");
}

BOOST_AUTO_TEST_CASE(yang_tong_mccarver_hines_beard_2006) {
	Cellml(TEST_MODELS("yang_tong_mccarver_hines_beard_2006.cellml"));
	GenerateAndCompare("yang_tong_mccarver_hines_beard_2006.cellml.var",
					   "yang_tong_mccarver_hines_beard_2006.cellml.bin");
}

BOOST_AUTO_TEST_CASE(Rybak_2006_with_static_instance_and_multiple_input) {
	Phml(TEST_MODELS("Rybak_2006_with_static_instance_and_multiple_input.isml"));
	GenerateAndCompare("Rybak_2006_with_static_instance_and_multiple_input.isml.var",
					   "Rybak_2006_with_static_instance_and_multiple_input.isml.bin");
}

BOOST_AUTO_TEST_CASE(hepatocyte_external) {
	Phml(TEST_MODELS("hepatocyte_external.isml"));
	boost::filesystem::remove("4d96c8de-d10a-48e2-a0e0-be9d74e58e78.db");
	GenerateAndCompare("hepatocyte_external.isml.var",
					   "hepatocyte_external.isml.bin");
}

BOOST_AUTO_TEST_CASE(hepatocyte_internal) {
	Phml(TEST_MODELS("hepatocyte_internal.isml"));
	boost::filesystem::remove("4d96c8de-d10a-48e2-a0e0-be9d74e58e78.db");
	boost::filesystem::remove("4d96c8de-d10a-48e2-a0e0-be9d74e58e78.xml");
	GenerateAndCompare("hepatocyte_internal.isml.var",
					   "hepatocyte_internal.isml.bin");
}

BOOST_AUTO_TEST_CASE(ringed_Beeler_Reuter_1977_model_with_static_instance) {
	Phml(TEST_MODELS("ringed_Beeler_Reuter_1977_model_with_static_instance.isml"));
	GenerateAndCompare("ringed_Beeler_Reuter_1977_model_with_static_instance.isml.var",
					   "ringed_Beeler_Reuter_1977_model_with_static_instance.isml.bin");
}

BOOST_AUTO_TEST_CASE(BIOMD0000000114) {
	Sbml(TEST_MODELS("BIOMD0000000114.xml"));
	GenerateAndCompare("BIOMD0000000114.xml.var",
					   "BIOMD0000000114.xml.bin");
}

BOOST_AUTO_TEST_CASE(BIOMD0000000152) {
	Sbml(TEST_MODELS("BIOMD0000000152.xml"));
	GenerateAndCompare("BIOMD0000000152.xml.var",
					   "BIOMD0000000152.xml.bin");
}

BOOST_AUTO_TEST_SUITE_END()
