/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/tr.h"

#include <boost/process/system.hpp>

#include "cli.pb.h"

#define BOOST_TEST_MODULE test_translate
#include "test.h"

struct F : public test::TemporaryWorkingDirectory {

	std::unique_ptr<cli::RunOption> GenerateOption(const char *model_filename,
												   const char *output_filename) {
		std::unique_ptr<cli::RunOption> option(new cli::RunOption);
		option->set_model_filename(model_filename);
		option->set_output_filename(output_filename);
		return option;
	}

	std::unique_ptr<char[]> input_;
};

BOOST_FIXTURE_TEST_SUITE(test_translate, F)

#ifdef __clang__
#define CC "clang"
#else
#define CC "gcc"
#endif

#define TRANSLATE_OK_CASE(name, filename) BOOST_AUTO_TEST_CASE(name) {	\
		PushWorkingDirectory(#name);									\
		std::unique_ptr<cli::RunOption> option(GenerateOption(TEST_MODELS(filename), filename ".c")); \
		test::StderrCapture sc;											\
		BOOST_CHECK(tr::Translate(*option));							\
		BOOST_CHECK_EQUAL(boost::process::system(CC " -std=c99 -Wall -W " filename ".c -lm"), EXIT_SUCCESS); \
		BOOST_CHECK(sc.Get().empty());									\
		PopWorkingDirectory();											\
	}

TRANSLATE_OK_CASE(BIOMD0000000114, "BIOMD0000000114.xml")
TRANSLATE_OK_CASE(BIOMD0000000152, "BIOMD0000000152.xml")
TRANSLATE_OK_CASE(fhn, "fhn.phml")
TRANSLATE_OK_CASE(fsk, "fsk.phml")
TRANSLATE_OK_CASE(hepatocyte_external, "hepatocyte_external.isml")
TRANSLATE_OK_CASE(hepatocyte_internal, "hepatocyte_internal.isml")
TRANSLATE_OK_CASE(lockwood_ewy_hermann_holford_2006, "lockwood_ewy_hermann_holford_2006.cellml")
TRANSLATE_OK_CASE(lorenz, "lorenz.phml")
TRANSLATE_OK_CASE(LR1_ring_reentry_initiate, "LR1-ring_reentry_initiate.isml")
TRANSLATE_OK_CASE(LR_pulse_189_164, "LR-pulse-189-164.isml")
TRANSLATE_OK_CASE(luo_rudy_1991, "luo_rudy_1991.isml")
TRANSLATE_OK_CASE(reduction, "reduction.phml")
TRANSLATE_OK_CASE(ringed_Beeler_Reuter_1977_model_with_static_instance, "ringed_Beeler_Reuter_1977_model_with_static_instance.isml")
TRANSLATE_OK_CASE(ringed_Luo_Rudy_1991_model_with_instance, "ringed_Luo_Rudy_1991_model_with_instance.isml")
TRANSLATE_OK_CASE(Rybak_2006_with_static_instance_and_multiple_input, "Rybak_2006_with_static_instance_and_multiple_input.isml")
TRANSLATE_OK_CASE(yang_tong_mccarver_hines_beard_2006, "yang_tong_mccarver_hines_beard_2006.cellml")

#undef CC

BOOST_AUTO_TEST_SUITE_END()
