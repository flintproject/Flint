/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/utf8string.h"

#include <libxml/xmlmemory.h>

#define BOOST_TEST_MODULE test_trim
#include "test.h"

struct F {
	F()
		: s(nullptr)
		, t(nullptr)
		, e(nullptr)
	{}

	~F() {
		xmlFree(s);
		xmlFree(e);
	}

	void CheckTrim(const char *input, const char *expected) {
		s = xmlCharStrdup(input);
		BOOST_CHECK_EQUAL(Trim(s, &t), 1);
		BOOST_CHECK(t != nullptr);
		e = xmlCharStrdup(expected);
		BOOST_CHECK(xmlStrEqual(t, e));
	}

	xmlChar *s;
	xmlChar *t;
	xmlChar *e;
};

BOOST_FIXTURE_TEST_SUITE(test_trim, F)

BOOST_AUTO_TEST_CASE(as_is)
{
	CheckTrim("as is", "as is");
}

BOOST_AUTO_TEST_CASE(empty)
{
	CheckTrim("", "");
}

BOOST_AUTO_TEST_CASE(space_only)
{
	CheckTrim(" \r\n\t", "");
}

BOOST_AUTO_TEST_CASE(left)
{
	CheckTrim("  foo bar", "foo bar");
}

BOOST_AUTO_TEST_CASE(right)
{
	CheckTrim("Okinawa, Japan\n", "Okinawa, Japan");
}

BOOST_AUTO_TEST_CASE(both)
{
	CheckTrim("  test ", "test");
}

BOOST_AUTO_TEST_SUITE_END()
