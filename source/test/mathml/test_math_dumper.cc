/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "mathml/math-dumper.h"

#include <sstream>
#define BOOST_TEST_MODULE test_math_dumper
#include "test.h"

struct GF {
	GF() {
		LIBXML_TEST_VERSION
		xmlInitParser();
	}
	~GF() {
		xmlCleanupParser();
	}
};

BOOST_GLOBAL_FIXTURE(GF);

class Handler {
public:
	int Handle(int i) const {return i;}
};

struct F {
	F() : text_reader_(nullptr),
		  os_()
	{}

	~F() {
		if (text_reader_) xmlFreeTextReader(text_reader_);
		os_.str("");
	}

	void ReadAndDump(const char *filename,
							const char *expected) {
		text_reader_ = xmlReaderForFile(filename, nullptr, 0);
		BOOST_CHECK(text_reader_ != nullptr);
		Handler handler;
		mathml::MathDumper dumper(text_reader_, &os_);
		int i = dumper.Read(&handler);
		BOOST_CHECK_EQUAL(i, 0);
		BOOST_CHECK_EQUAL(os_.str().c_str(), expected);
	}

	void ReadAndError(const char *filename, const char *expected)
	{
		text_reader_ = xmlReaderForFile(filename, nullptr, 0);
		BOOST_REQUIRE(text_reader_);
		Handler handler;
		mathml::MathDumper dumper(text_reader_, &os_);
		test::StderrCapture capture;
		int i = dumper.Read(&handler);
		BOOST_CHECK(i < 0);
		BOOST_CHECK_EQUAL(capture.Get(), expected);
	}

	xmlTextReaderPtr text_reader_;
	std::ostringstream os_;
};

BOOST_FIXTURE_TEST_SUITE(test_math_dumper, F)

BOOST_AUTO_TEST_CASE(mathml_cosh)
{
	ReadAndDump(TEST_MODELS("mathml_cosh.xml"),
				" math (eq (cosh %x) (divide (plus (exp %x) (exp (minus %x))) 2))");
}

BOOST_AUTO_TEST_CASE(mathml_matrix)
{
	ReadAndDump(TEST_MODELS("mathml_matrix.xml"),
				" math (eq %M (matrix (matrixrow 0 1 0) (matrixrow 0 0 1) (matrixrow 1 0 0)))");
}

BOOST_AUTO_TEST_CASE(mathml_piecewise)
{
	ReadAndDump(TEST_MODELS("mathml_piecewise.xml"),
				" math (eq (abs %x) (piecewise (piece (minus %x) (lt %x 0)) (piece 0 (eq %x 0)) (piece %x (gt %x 0))))");
}

BOOST_AUTO_TEST_CASE(mathml_sinh)
{
	ReadAndDump(TEST_MODELS("mathml_sinh.xml"),
				" math (eq (sinh %x) (divide (minus (exp %x) (exp (minus %x))) 2))");
}

BOOST_AUTO_TEST_CASE(mathml_tanh)
{
	ReadAndDump(TEST_MODELS("mathml_tanh.xml"),
				" math (eq (tanh %x) (divide (sinh %x) (cosh %x)))");
}

BOOST_AUTO_TEST_CASE(mathml_vector)
{
	ReadAndDump(TEST_MODELS("mathml_vector.xml"),
				" math (eq %V (vector 1 2.34e-10 3/4 %x))");
}

BOOST_AUTO_TEST_CASE(x_mathml_ci)
{
	ReadAndError(TEST_MODELS("x-mathml-ci.xml"),
				 "invalid body of <ci>: 42\n");
}

BOOST_AUTO_TEST_CASE(x_mathml_cn)
{
	ReadAndError(TEST_MODELS("x-mathml-cn.xml"),
				 "invalid body of <cn>: 1!?\n");
}

BOOST_AUTO_TEST_SUITE_END()
