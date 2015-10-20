/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.hh"

#include <memory>

#define BOOST_TEST_MODULE test_path
#include "test.hh"

struct F {
	std::unique_ptr<char[]> path_;
};

BOOST_FIXTURE_TEST_SUITE(test_path, F)

BOOST_AUTO_TEST_CASE(BuildPath)
{
	path_.reset(job::BuildPath("1", 0));
	BOOST_CHECK_EQUAL(path_.get(), "1/00/00/00/00");
	path_.reset(job::BuildPath("5", 3000));
	BOOST_CHECK_EQUAL(path_.get(), "5/00/00/0b/b8");
	path_.reset(job::BuildPath("foo/42", 0x7FFFFFFF));
	BOOST_CHECK_EQUAL(path_.get(), "foo/42/7f/ff/ff/ff");
}

BOOST_AUTO_TEST_SUITE_END()
