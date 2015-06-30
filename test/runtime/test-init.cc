/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "runtime.hh"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include "compiler/bcc.h"
#include "database.h"
#include "db/driver.hh"
#include "db/name-inserter.h"
#include "db/query.h"
#include "db/tac-inserter.hh"
#include "layout.hh"

#define BOOST_TEST_MODULE test_init
#include "test.hh"


namespace {

const char kDefaultSpaceId[] = "00000000-0000-0000-0000-000000000000";

void CheckOutput(double expected)
{
	static const int kBufferSize = 56;

	FILE *fp = std::fopen("output", "rb");
	BOOST_CHECK(fp);
	char buf[kBufferSize];
	size_t s = fread(buf, kBufferSize, 1, fp);
	fclose(fp);
	BOOST_CHECK_EQUAL(s, 1u);
	double x;
	memcpy(&x, &buf[48], sizeof(x));
	BOOST_CHECK_EQUAL(x, expected);
}

}

struct F {
	F()
		: driver_(":memory:")
	{
		BOOST_REQUIRE_EQUAL(CreateSingleton(driver_.db()), 1);
		db::NameInserter ni("names", driver_.db());
		BOOST_REQUIRE(ni.InsertName(kDefaultSpaceId, 'v', 1, "a"));
		BOOST_REQUIRE(ni.InsertName(kDefaultSpaceId, 'v', 2, "b"));
		BOOST_REQUIRE(ni.InsertName(kDefaultSpaceId, 'v', 3, "x"));
		BOOST_REQUIRE(layout::Generate(driver_.db(), "layout"));
		BOOST_REQUIRE_EQUAL(CreateTable(driver_.db(), "tacs", "(uuid TEXT, name TEXT, nod INTEGER, body TEXT)"), 1);
		BOOST_REQUIRE_EQUAL(SaveNol(1, driver_.db()), 1);
	}

	~F()
	{
		boost::filesystem::remove("layout");
		boost::filesystem::remove("bc");
		boost::filesystem::remove("output");
	}

	void Insert(const char *name, int nod, const char *body)
	{
		db::TacInserter ti(driver_.db());
		BOOST_CHECK(ti.Insert(kDefaultSpaceId, name, nod, body));
	}

	db::Driver driver_;
};

#define SETUP(a, b) do {						\
		test::Sql sql(driver_.db());			\
		sql.Exec("DELETE FROM tacs");			\
		Insert("%a#0", 1,						\
			   "  loadi $0 " #a "\n"			\
			   "  store %a#0 $0\n");			\
		Insert("%b#0", 1,						\
			   "  loadi $0 " #b "\n"			\
			   "  store %b#0 $0\n");			\
	} while (0)

#define TESTCASE1(f, a, expected) do {									\
		SETUP(a, 0);													\
		Insert("%x#0", 2,												\
			   "  load $1 %a#0\n"										\
			   "  $0 = (" #f " $1)\n"									\
			   "  store %x#0 $0\n");									\
		std::ofstream ofs("bc", std::ios::out|std::ios::binary);		\
		BOOST_REQUIRE(compiler::bcc::Bcc(driver_.db(), &ofs));			\
		ofs.close();													\
		BOOST_CHECK(runtime::Init(driver_.db(), "layout", "bc", "output"));	\
		CheckOutput(expected);											\
	} while (0)

#define TESTCASE2(f, a, b, expected) do {								\
		SETUP(a, b);													\
		Insert("%x#0", 3,												\
			   "  load $1 %a#0\n"										\
			   "  load $2 %b#0\n"										\
			   "  $0 = (" #f " $1 $2)\n"								\
			   "  store %x#0 $0\n");									\
		std::ofstream ofs("bc", std::ios::out|std::ios::binary);		\
		BOOST_REQUIRE(compiler::bcc::Bcc(driver_.db(), &ofs));			\
		ofs.close();													\
		BOOST_CHECK(runtime::Init(driver_.db(), "layout", "bc", "output"));	\
		CheckOutput(expected);											\
	} while (0)

BOOST_FIXTURE_TEST_SUITE(test_init, F)

BOOST_AUTO_TEST_CASE(abs)
{
	TESTCASE1(abs, 0, 0);
	TESTCASE1(abs, 1, 1);
	TESTCASE1(abs, -1, 1);
}

BOOST_AUTO_TEST_CASE(arccos)
{
	TESTCASE1(arccos, 1, 0);
}

BOOST_AUTO_TEST_CASE(arccosh)
{
	TESTCASE1(arccosh, 1, 0);
}

BOOST_AUTO_TEST_CASE(arccot)
{
	TESTCASE1(arccot, 0, 1.5707963267948966); // pi/2
	TESTCASE1(arccot, 1, .785398163397448309); // pi/4
	TESTCASE1(arccot, -1, -.785398163397448309); // -pi/4
}

BOOST_AUTO_TEST_CASE(arccoth)
{
	TESTCASE1(arccoth, 5, .20273255405408219);
	TESTCASE1(arccoth, -5, -.20273255405408219);
}

BOOST_AUTO_TEST_CASE(arccsc)
{
	TESTCASE1(arccsc, 1, 1.5707963267948966); // pi/2
	TESTCASE1(arccsc, -1, -1.5707963267948966); // -pi/2
}

BOOST_AUTO_TEST_CASE(arccsch)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(arcsec)
{
	TESTCASE1(arcsec, 1, 0);
}

BOOST_AUTO_TEST_CASE(arcsech)
{
	TESTCASE1(arcsech, 1, 0);
}

BOOST_AUTO_TEST_CASE(arcsin)
{
	TESTCASE1(arcsin, 0, 0);
}

BOOST_AUTO_TEST_CASE(arcsinh)
{
	TESTCASE1(arcsinh, 0, 0);
}

BOOST_AUTO_TEST_CASE(arctan)
{
	TESTCASE1(arctan, 0, 0);
}

BOOST_AUTO_TEST_CASE(arctanh)
{
	TESTCASE1(arctanh, 0, 0);
}

BOOST_AUTO_TEST_CASE(ceiling)
{
	TESTCASE1(ceiling, 0, 0);
	TESTCASE1(ceiling, 1, 1);
	TESTCASE1(ceiling, 1.5, 2);
	TESTCASE1(ceiling, -0.8, 0);
	TESTCASE1(ceiling, -1, -1);
}

BOOST_AUTO_TEST_CASE(cos)
{
	TESTCASE1(cos, 0, 1);
}

BOOST_AUTO_TEST_CASE(cosh)
{
	TESTCASE1(cosh, 0, 1);
}

BOOST_AUTO_TEST_CASE(cot)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(coth)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(csc)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(csch)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(divide)
{
	TESTCASE2(divide, 1, 2, 0.5);
	TESTCASE2(divide, -1, 2, -0.5);
	TESTCASE2(divide, 1, -2, -0.5);
	TESTCASE2(divide, -1, -2, 0.5);
}

BOOST_AUTO_TEST_CASE(eq)
{
	TESTCASE2(eq, 0, 0, 1);
	TESTCASE2(eq, 0, 1, 0);
	TESTCASE2(eq, 1, 0, 0);
	TESTCASE2(eq, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(exp)
{
	TESTCASE1(exp, 0, 1);
}

BOOST_AUTO_TEST_CASE(floor)
{
	TESTCASE1(floor, 0, 0);
	TESTCASE1(floor, 0.5, 0);
	TESTCASE1(floor, 1, 1);
	TESTCASE1(floor, -1, -1);
	TESTCASE1(floor, -1.2, -2);
}

BOOST_AUTO_TEST_CASE(geq)
{
	TESTCASE2(geq, 0, 0, 1);
	TESTCASE2(geq, 0, 1, 0);
	TESTCASE2(geq, 1, 0, 1);
	TESTCASE2(geq, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(gt)
{
	TESTCASE2(gt, 0, 0, 0);
	TESTCASE2(gt, 0, 1, 0);
	TESTCASE2(gt, 1, 0, 1);
	TESTCASE2(gt, 1, 1, 0);
}

BOOST_AUTO_TEST_CASE(leq)
{
	TESTCASE2(leq, 0, 0, 1);
	TESTCASE2(leq, 0, 1, 1);
	TESTCASE2(leq, 1, 0, 0);
	TESTCASE2(leq, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(ln)
{
	TESTCASE1(ln, 1, 0);
}

BOOST_AUTO_TEST_CASE(log10)
{
	TESTCASE1(log10, 1, 0);
	TESTCASE1(log10, 10, 1);
	TESTCASE1(log10, 100, 2);
	TESTCASE1(log10, 0.1, -1);
	TESTCASE1(log10, 0.01, -2);
}

BOOST_AUTO_TEST_CASE(log)
{
	TESTCASE2(log, 2, 1, 0);
	TESTCASE2(log, 2, 2, 1);
	TESTCASE2(log, 2, 8, 3);
	TESTCASE2(log, 2, 0.5, -1);
	TESTCASE2(log, 3, 1, 0);
	TESTCASE2(log, 3, 9, 2);
}

BOOST_AUTO_TEST_CASE(lt)
{
	TESTCASE2(lt, 0, 0, 0);
	TESTCASE2(lt, 0, 1, 1);
	TESTCASE2(lt, 1, 0, 0);
	TESTCASE2(lt, 1, 1, 0);
}

BOOST_AUTO_TEST_CASE(max)
{
	TESTCASE2(max, 0, 0, 0);
	TESTCASE2(max, 0, 1, 1);
	TESTCASE2(max, 1, 0, 1);
	TESTCASE2(max, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(min)
{
	TESTCASE2(min, 0, 0, 0);
	TESTCASE2(min, 0, 1, 0);
	TESTCASE2(min, 1, 0, 0);
	TESTCASE2(min, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(neq)
{
	TESTCASE2(neq, 0, 0, 0);
	TESTCASE2(neq, 0, 1, 1);
	TESTCASE2(neq, 1, 0, 1);
	TESTCASE2(neq, 1, 1, 0);
}

BOOST_AUTO_TEST_CASE(plus)
{
	TESTCASE2(plus, 0, 0, 0);
	TESTCASE2(plus, 0, 1, 1);
	TESTCASE2(plus, 1, 0, 1);
	TESTCASE2(plus, 1, -1, 0);
	TESTCASE2(plus, 1, 2, 3);
	TESTCASE2(plus, -1, -2, -3);
}

BOOST_AUTO_TEST_CASE(power)
{
	TESTCASE2(power, 0, 0, 1);
	TESTCASE2(power, 0, 1, 0);
	TESTCASE2(power, 1, 0, 1);
	TESTCASE2(power, 1, 1, 1);
	TESTCASE2(power, 2, 3, 8);
}

BOOST_AUTO_TEST_CASE(rem)
{
	TESTCASE2(rem, 0, 1, 0);
	TESTCASE2(rem, 0, 2, 0);
	TESTCASE2(rem, 1, 1, 0);
	TESTCASE2(rem, 1, 2, 1);
	TESTCASE2(rem, 2, 3, 2);
	TESTCASE2(rem, 3, 2, 1);
}

BOOST_AUTO_TEST_CASE(sec)
{
	TESTCASE1(sec, 0, 1);
}

BOOST_AUTO_TEST_CASE(sech)
{
	TESTCASE1(sech, 0, 1);
}

BOOST_AUTO_TEST_CASE(sin)
{
	TESTCASE1(sin, 0, 0);
}

BOOST_AUTO_TEST_CASE(sinh)
{
	TESTCASE1(sinh, 0, 0);
}

BOOST_AUTO_TEST_CASE(tan)
{
	TESTCASE1(tan, 0, 0);
}

BOOST_AUTO_TEST_CASE(tanh)
{
	TESTCASE1(tanh, 0, 0);
}

BOOST_AUTO_TEST_CASE(times)
{
	TESTCASE2(times, 0, 0, 0);
	TESTCASE2(times, 0, 1, 0);
	TESTCASE2(times, 1, 0, 0);
	TESTCASE2(times, 2, 3, 6);
	TESTCASE2(times, 3, 2, 6);
	TESTCASE2(times, -2, 3, -6);
	TESTCASE2(times, 2, -3, -6);
	TESTCASE2(times, -2, -3, 6);
}

BOOST_AUTO_TEST_SUITE_END()
