/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "run.hh"

#include "cli.pb.h"

#define BOOST_TEST_MODULE test_run
#include "test.hh"

struct F : public test::TemporaryWorkingDirectory {

	int GenerateInput(const char *model_filename, const char *output_filename) {
		cli::RunOption option;
		option.set_model_filename(model_filename);
		option.set_output_filename(output_filename);
		int s = option.ByteSize();
		input_.reset(new char[s]);
		BOOST_REQUIRE(option.SerializeToArray(input_.get(), s));
		return s;
	}

	std::unique_ptr<char[]> input_;
};

BOOST_FIXTURE_TEST_SUITE(test_run, F)

BOOST_AUTO_TEST_CASE(fhn) {
	PushWorkingDirectory("fhn");
	int s = GenerateInput(TEST_MODELS("fhn.phml"), "fhn.out");
	test::StderrCapture sc;
	BOOST_CHECK(run::Run(input_.get(), s));
	BOOST_CHECK(sc.Get().empty());
	PopWorkingDirectory();
}

BOOST_AUTO_TEST_CASE(x_missing_name) {
	PushWorkingDirectory("x-missing-name");
	int s = GenerateInput(TEST_MODELS("x-missing-name.phml"), "x-missing-name.out");
	test::StderrCapture sc;
	BOOST_CHECK(!run::Run(input_.get(), s));
	BOOST_CHECK_EQUAL(sc.Get(),
					  "failed to find variable: y\n"
					  " in c5e5c13e-34c9-4e2b-b8fd-b5fe98807134\n");
	PopWorkingDirectory();
}

BOOST_AUTO_TEST_SUITE_END()
