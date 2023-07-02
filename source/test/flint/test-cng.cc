/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/utf8string.h"

#include <libxml/xmlmemory.h>

#define BOOST_TEST_MODULE test_cng
#include "test.h"

struct F {
	F()
		: s(nullptr)
	{}

	~F() {
		xmlFree(s);
	}

	void CheckNonGraphic(const char *input, int expected) {
		s = xmlCharStrdup(input);
		BOOST_CHECK_EQUAL(ContainNonGraphic(s), expected);
	}

	xmlChar *s;
};

BOOST_FIXTURE_TEST_SUITE(test_cng, F)

BOOST_AUTO_TEST_CASE(space)
{
	CheckNonGraphic("Oct 2015", 1);
}

BOOST_AUTO_TEST_CASE(non_printable)
{
	CheckNonGraphic("\x7f", 1);
}

BOOST_AUTO_TEST_CASE(negative)
{
	CheckNonGraphic("!\"#$%&'()*+,-./"
					"0123456789"
					":;<=>?@"
					"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"[\\]^_`"
					"abcdefghijklmnopqrstuvwxyz"
					"{|}~",
					0);
}

BOOST_AUTO_TEST_SUITE_END()
