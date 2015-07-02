/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run/spec.hh"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include "cellml.hh"
#include "database.h"
#include "phml.hh"
#include "sbml.hh"

#define BOOST_TEST_MODULE test_spec
#include "test.hh"

struct F : public test::MemoryFixture {

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
		FILE *ofp = std::fopen(output, "wb");
		BOOST_REQUIRE(ofp != nullptr);
		BOOST_REQUIRE(run::Spec(driver_.db(), ofp));
		std::fclose(ofp);

		boost::filesystem::path fp(__FILE__);
		boost::filesystem::path ep = fp.parent_path();
		ep /= "spec";
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
};

BOOST_FIXTURE_TEST_SUITE(test_spec, F)

BOOST_AUTO_TEST_CASE(lockwood_ewy_hermann_holford_2006) {
	Cellml(TEST_MODELS("lockwood_ewy_hermann_holford_2006.cellml"));
	GenerateAndCompare("lockwood_ewy_hermann_holford_2006.cellml.spec",
					   "lockwood_ewy_hermann_holford_2006.cellml.txt");
}

BOOST_AUTO_TEST_CASE(yang_tong_mccarver_hines_beard_2006) {
	Cellml(TEST_MODELS("yang_tong_mccarver_hines_beard_2006.cellml"));
	GenerateAndCompare("yang_tong_mccarver_hines_beard_2006.cellml.spec",
					   "yang_tong_mccarver_hines_beard_2006.cellml.txt");
}

BOOST_AUTO_TEST_CASE(Rybak_2006_with_static_instance_and_multiple_input) {
	Phml(TEST_MODELS("Rybak_2006_with_static_instance_and_multiple_input.isml"));
	GenerateAndCompare("Rybak_2006_with_static_instance_and_multiple_input.isml.spec",
					   "Rybak_2006_with_static_instance_and_multiple_input.isml.txt");
}

BOOST_AUTO_TEST_CASE(hepatocyte_external) {
	Phml(TEST_MODELS("hepatocyte_external.isml"));
	boost::filesystem::remove("4d96c8de-d10a-48e2-a0e0-be9d74e58e78.db");
	GenerateAndCompare("hepatocyte_external.isml.spec",
					   "hepatocyte_external.isml.txt");
}

BOOST_AUTO_TEST_CASE(hepatocyte_internal) {
	Phml(TEST_MODELS("hepatocyte_internal.isml"));
	boost::filesystem::remove("4d96c8de-d10a-48e2-a0e0-be9d74e58e78.db");
	boost::filesystem::remove("4d96c8de-d10a-48e2-a0e0-be9d74e58e78.xml");
	GenerateAndCompare("hepatocyte_internal.isml.spec",
					   "hepatocyte_internal.isml.txt");
}

BOOST_AUTO_TEST_CASE(ringed_Beeler_Reuter_1977_model_with_static_instance) {
	Phml(TEST_MODELS("ringed_Beeler_Reuter_1977_model_with_static_instance.isml"));
	GenerateAndCompare("ringed_Beeler_Reuter_1977_model_with_static_instance.isml.spec",
					   "ringed_Beeler_Reuter_1977_model_with_static_instance.isml.txt");
}

BOOST_AUTO_TEST_CASE(BIOMD0000000114) {
	Sbml(TEST_MODELS("BIOMD0000000114.xml"));
	GenerateAndCompare("BIOMD0000000114.xml.spec",
					   "BIOMD0000000114.xml.txt");
}

BOOST_AUTO_TEST_CASE(BIOMD0000000152) {
	Sbml(TEST_MODELS("BIOMD0000000152.xml"));
	GenerateAndCompare("BIOMD0000000152.xml.spec",
					   "BIOMD0000000152.xml.txt");
}

BOOST_AUTO_TEST_SUITE_END()
