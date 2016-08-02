/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load.h"

#define BOOST_TEST_MODULE test_load
#include "test.h"

BOOST_FIXTURE_TEST_SUITE(test_load, test::TemporaryWorkingDirectory)

BOOST_AUTO_TEST_CASE(empty) {
	PushWorkingDirectory("empty");
	test::StderrCapture capture;
	std::vector<double> data;
	BOOST_CHECK(!load::Load(TEST_MODELS("empty.phml"), load::kExec, 0, &data));
	BOOST_CHECK_EQUAL(capture.Get(), "no variables found, possibly due to empty model or no instances\n");
	PopWorkingDirectory();
}

BOOST_AUTO_TEST_CASE(exponential) {
	boost::filesystem::path p0(original_path_);
	boost::filesystem::path p1(original_path_);
	p0 /= "exponential";
	p1 /= "exponential_with_seed";
	if (boost::filesystem::exists(p0))
		boost::filesystem::remove_all(p0);
	if (boost::filesystem::exists(p1))
		boost::filesystem::remove_all(p1);
	BOOST_CHECK(boost::filesystem::create_directory(p0));
	BOOST_CHECK(boost::filesystem::create_directory(p1));
	boost::filesystem::current_path(p0);
	std::vector<double> data0;
	BOOST_CHECK(load::Load(TEST_MODELS("exponential.isml"), load::kExec, 0, &data0));
	boost::filesystem::current_path(p1);
	std::vector<double> data1;
	BOOST_CHECK(load::Load(TEST_MODELS("exponential_with_seed.isml"), load::kExec, 0, &data1));
	BOOST_CHECK_EQUAL(data0.size(), data1.size());
	BOOST_TEST(data0 != data1);
}

BOOST_AUTO_TEST_CASE(fsk) {
	boost::filesystem::path p0(original_path_);
	boost::filesystem::path p1(original_path_);
	p0 /= "fsk";
	p1 /= "fsk-plus-no-instance";
	if (boost::filesystem::exists(p0))
		boost::filesystem::remove_all(p0);
	if (boost::filesystem::exists(p1))
		boost::filesystem::remove_all(p1);
	BOOST_CHECK(boost::filesystem::create_directory(p0));
	BOOST_CHECK(boost::filesystem::create_directory(p1));
	boost::filesystem::current_path(p0);
	std::vector<double> data0;
	BOOST_CHECK(load::Load(TEST_MODELS("fsk.phml"), load::kExec, 0, &data0));
	boost::filesystem::current_path(p1);
	std::vector<double> data1;
	BOOST_CHECK(load::Load(TEST_MODELS("fsk-plus-no-instance.phml"), load::kExec, 0, &data1));
	BOOST_TEST(data0 == data1);
	boost::filesystem::remove_all(p0);
	boost::filesystem::remove_all(p1);
}

BOOST_AUTO_TEST_CASE(timeseries) {
	PushWorkingDirectory("timeseries");
	test::StderrCapture capture;
	std::vector<double> data;
	BOOST_CHECK(load::Load(TEST_MODELS("timeseries.phml"), load::kExec, 0, &data));
	BOOST_CHECK_EQUAL(capture.Get(), "");
	PopWorkingDirectory();
}

BOOST_AUTO_TEST_CASE(x_static_only) {
	PushWorkingDirectory("x-static-only");
	test::StderrCapture capture;
	std::vector<double> data;
	BOOST_CHECK(!load::Load(TEST_MODELS("x-static-only.phml"), load::kExec, 0, &data));
	BOOST_CHECK_EQUAL(capture.Get(), "no dependent variables found\n");
	PopWorkingDirectory();
}

BOOST_AUTO_TEST_SUITE_END()
