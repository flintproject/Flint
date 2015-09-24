/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load.hh"

#define BOOST_TEST_MODULE test_load
#include "test.hh"

struct F : public test::MemoryFixture {

	F()
		: path_(boost::filesystem::current_path())
	{}

	~F() {
		boost::filesystem::current_path(path_);
	}

	boost::filesystem::path path_;
};

BOOST_FIXTURE_TEST_SUITE(test_load, F)

BOOST_AUTO_TEST_CASE(exponential) {
	boost::filesystem::path p0(path_);
	boost::filesystem::path p1(path_);
	p0 /= "exponential";
	p1 /= "exponential_with_seed";
	if (boost::filesystem::exists(p0))
		boost::filesystem::remove_all(p0);
	if (boost::filesystem::exists(p1))
		boost::filesystem::remove_all(p1);
	BOOST_CHECK(boost::filesystem::create_directory(p0));
	BOOST_CHECK(boost::filesystem::create_directory(p1));
	boost::filesystem::current_path(p0);
	BOOST_CHECK(load::Load(TEST_MODELS("exponential.isml"), load::kExec));
	boost::filesystem::current_path(p1);
	BOOST_CHECK(load::Load(TEST_MODELS("exponential_with_seed.isml"), load::kExec));
	boost::filesystem::path p0_init(p0);
	boost::filesystem::path p1_init(p1);
	p0_init /= "init";
	p1_init /= "init";
	test::CheckDifference(p0_init, p1_init);
	boost::filesystem::remove_all(p0);
	boost::filesystem::remove_all(p1);
}

BOOST_AUTO_TEST_SUITE_END()
