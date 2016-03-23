/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run.h"

#include "cli.pb.h"

#define BOOST_TEST_MODULE test_run
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

BOOST_FIXTURE_TEST_SUITE(test_run, F)

BOOST_AUTO_TEST_CASE(fhn) {
	PushWorkingDirectory("fhn");
	std::unique_ptr<cli::RunOption> option(GenerateOption(TEST_MODELS("fhn.phml"), "fhn.out"));
	test::StderrCapture sc;
	BOOST_CHECK(run::Run(*option));
	BOOST_CHECK(sc.Get().empty());
	PopWorkingDirectory();
}

BOOST_AUTO_TEST_CASE(x_missing_name) {
	PushWorkingDirectory("x-missing-name");
	std::unique_ptr<cli::RunOption> option(GenerateOption(TEST_MODELS("x-missing-name.phml"), "x-missing-name.out"));
	test::StderrCapture sc;
	BOOST_CHECK(!run::Run(*option));
	BOOST_CHECK_EQUAL(sc.Get(),
					  "failed to find variable: y\n"
					  " in c5e5c13e-34c9-4e2b-b8fd-b5fe98807134\n");
	PopWorkingDirectory();
}

BOOST_AUTO_TEST_SUITE_END()
