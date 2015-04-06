/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "base/rational.h"
#define BOOST_TEST_MODULE base/test_rational
#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE(FromString)
{
	boost::rational<int> r(0);

	BOOST_CHECK(!base::Rational<int>::FromString("", r));
	BOOST_CHECK(!base::Rational<int>::FromString(".", r));
	BOOST_CHECK(!base::Rational<int>::FromString("e", r));
	BOOST_CHECK(!base::Rational<int>::FromString("foo", r));
	BOOST_CHECK(!base::Rational<int>::FromString("++1", r));
	BOOST_CHECK(!base::Rational<int>::FromString("+-1", r));
	BOOST_CHECK(!base::Rational<int>::FromString("-+1", r));
	BOOST_CHECK(!base::Rational<int>::FromString("--1", r));

	BOOST_CHECK(base::Rational<int>::FromString("1", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(1));
	BOOST_CHECK(base::Rational<int>::FromString("256", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(256));
	BOOST_CHECK(base::Rational<int>::FromString("1.020", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(102, 100));
	BOOST_CHECK(base::Rational<int>::FromString("1.5", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(15, 10));
	BOOST_CHECK(base::Rational<int>::FromString(".5", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(5, 10));
	BOOST_CHECK(base::Rational<int>::FromString(".5e8", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(50000000));
	BOOST_CHECK(base::Rational<int>::FromString("1e-05", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(1, 100000));
	BOOST_CHECK(base::Rational<int>::FromString("31.0e-3", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(31, 1000));
	BOOST_CHECK(base::Rational<int>::FromString("0.002e7", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(20000));
	BOOST_CHECK(base::Rational<int>::FromString("-3", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(-3));
	BOOST_CHECK(base::Rational<int>::FromString("-0.2", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(-1, 5));
	BOOST_CHECK(base::Rational<int>::FromString("-.1", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(-1, 10));
	BOOST_CHECK(base::Rational<int>::FromString("+3", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(3));
	BOOST_CHECK(base::Rational<int>::FromString("+0.2", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(1, 5));
	BOOST_CHECK(base::Rational<int>::FromString("+.1", r));
	BOOST_CHECK_EQUAL(r, boost::rational<int>(1, 10));
}
