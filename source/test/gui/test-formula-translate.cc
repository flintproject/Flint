/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "gui/formula.h"

#define BOOST_TEST_MODULE test_gui_formula_translate
#include "test.h"

using gui::Formula;

struct F {
	void Translate(const char *input, const char *expected) {
		std::ostringstream es;
		std::unique_ptr<Formula> f(Formula::FromUtf8(input, es));
		BOOST_CHECK_EQUAL(es.str().c_str(), "");
		BOOST_REQUIRE(f);
		std::ostringstream os;
		f->WriteMathML("m", os);
		BOOST_CHECK_EQUAL(os.str().c_str(), expected);
	}
};

BOOST_FIXTURE_TEST_SUITE(test_gui_formula_translate, F)

BOOST_AUTO_TEST_CASE(Compound) {
	Translate("cos(a / b) - sin(x * y)",
			  "<m:apply><m:minus/><m:apply><m:cos/><m:apply><m:divide/><m:ci>a</m:ci><m:ci>b</m:ci></m:apply></m:apply><m:apply><m:sin/><m:apply><m:times/><m:ci>x</m:ci><m:ci>y</m:ci></m:apply></m:apply></m:apply>");
	Translate("(cos(a / b) - sin(x * y))",
			  "<m:apply><m:minus/><m:apply><m:cos/><m:apply><m:divide/><m:ci>a</m:ci><m:ci>b</m:ci></m:apply></m:apply><m:apply><m:sin/><m:apply><m:times/><m:ci>x</m:ci><m:ci>y</m:ci></m:apply></m:apply></m:apply>");
	Translate("- (cos(a / b) - sin(x * y))",
			  "<m:apply><m:minus/><m:apply><m:minus/><m:apply><m:cos/><m:apply><m:divide/><m:ci>a</m:ci><m:ci>b</m:ci></m:apply></m:apply><m:apply><m:sin/><m:apply><m:times/><m:ci>x</m:ci><m:ci>y</m:ci></m:apply></m:apply></m:apply></m:apply>");
	Translate("- (cos(a / b) - sin(x * y)) + 0.1",
			  "<m:apply><m:plus/><m:apply><m:minus/><m:apply><m:minus/><m:apply><m:cos/><m:apply><m:divide/><m:ci>a</m:ci><m:ci>b</m:ci></m:apply></m:apply><m:apply><m:sin/><m:apply><m:times/><m:ci>x</m:ci><m:ci>y</m:ci></m:apply></m:apply></m:apply></m:apply><m:cn>0.1</m:cn></m:apply>");
}

BOOST_AUTO_TEST_CASE(Issue17) {
	Translate("x+1", "<m:apply><m:plus/><m:ci>x</m:ci><m:cn>1</m:cn></m:apply>");
	Translate("x-1", "<m:apply><m:minus/><m:ci>x</m:ci><m:cn>1</m:cn></m:apply>");
	Translate("x-(-1)", "<m:apply><m:minus/><m:ci>x</m:ci><m:cn>-1</m:cn></m:apply>");
	Translate("x+ -1", "<m:apply><m:plus/><m:ci>x</m:ci><m:cn>-1</m:cn></m:apply>");
	Translate("x- -1", "<m:apply><m:minus/><m:ci>x</m:ci><m:cn>-1</m:cn></m:apply>");
}

BOOST_AUTO_TEST_SUITE_END()
