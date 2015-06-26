/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.hh"

#include "database.h"
#include "db/driver.hh"

#define BOOST_TEST_MODULE test_read
#include "test.hh"

struct F {

	F()
		: driver_(":memory:")
	{}

	void ReadAndError(const char *file, const char *expected)
	{
		BOOST_REQUIRE_EQUAL(SaveGivenFile(driver_.db(), file), 1);
		test::StderrCapture capture;
		BOOST_CHECK(!phml::Read(driver_.db()));
		BOOST_CHECK_EQUAL(capture.Get(), expected);
	}

	db::Driver driver_;
};

BOOST_FIXTURE_TEST_SUITE(test_read, F)

BOOST_AUTO_TEST_CASE(double_pendulum) {
	ReadAndError(TEST_MODELS("double_pendulum.isml"),
				 "missing unit-id of <element>\n");
}

BOOST_AUTO_TEST_CASE(self_circular_unit) {
	ReadAndError(TEST_MODELS("self-circular-unit.phml"),
				 "unit with unit-id 8 is ill-defined by <element> with its own unit-id\n");
}

BOOST_AUTO_TEST_CASE(state_to_static) {
	ReadAndError(TEST_MODELS("state-to-static.phml"),
				 "found invalid edge from a state to a static-parameter\n"
				 "  from\n"
				 "    port-id: 1\n"
				 "    module-id: 3e10b8f4-75f8-11e4-ae2e-d39dff277ccb\n"
				 "  to\n"
				 "    port-id: 1\n"
				 "    module-id: 3cb504a6-75f8-11e4-85b6-8b3365a37bec\n");
}

BOOST_AUTO_TEST_CASE(swapped_definitions) {
	ReadAndError(TEST_MODELS("swapped-definitions.phml"),
				 "invalid definition of <implementation> for y in 22ddc4e8-c6ff-11e4-a78c-576e48c58a72\n");
}

BOOST_AUTO_TEST_CASE(variable_to_static) {
	ReadAndError(TEST_MODELS("variable-to-static.phml"),
				 "found invalid edge from a variable-parameter to a static-parameter\n"
				 "  from\n"
				 "    port-id: 1\n"
				 "    module-id: 85ded370-75f6-11e4-ac26-ff6c3d89c805\n"
				 "  to\n"
				 "    port-id: 1\n"
				 "    module-id: 57a14bc8-75f6-11e4-aa25-37030d28703a\n");
}

BOOST_AUTO_TEST_CASE(x_capsulated_by) {
	ReadAndError(TEST_MODELS("x-capsulated-by.phml"),
				 "module of module-id daa7dafe-7641-4280-9559-2e080e93578b is capsulated by unknown capsule module: 9114256a-d9bf-11e4-8933-5b24fd24d827\n");
}

BOOST_AUTO_TEST_SUITE_END()
