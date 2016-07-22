/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.h"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include "bc/index.h"
#include "compiler/bcc.h"
#include "database.h"
#include "db/helper.h"
#include "db/query.h"
#include "db/tac-inserter.h"
#include "db/variable-inserter.h"
#include "layout.h"

#define BOOST_TEST_MODULE test_evolve
#include "test.h"


namespace {

void CheckOutput(double expected)
{
	static const int kBufferSize = sizeof(double)*(kOffsetBase+3);

	FILE *fp = std::fopen("output", "rb");
	BOOST_REQUIRE(fp);
	char buf[kBufferSize];
	size_t s = fread(buf, kBufferSize, 1, fp);
	fclose(fp);
	BOOST_REQUIRE_EQUAL(s, 1u);
	double x;
	std::memcpy(&x, &buf[sizeof(double)*(kOffsetBase+2)], sizeof(x));
	BOOST_CHECK_EQUAL(x, expected);
}

}

struct F : public test::MemoryFixture {
	F()
	{
		BOOST_REQUIRE_EQUAL(CreateSingleton(driver_.db()), 1);
		test::Sql sql(driver_.db());
		sql.Exec("UPDATE config SET method = 'euler', length = '0.01', step = '0.01', granularity = '1'");
		db::VariableInserter vi("variables", false, driver_.db());
		BOOST_REQUIRE(vi.Insert('v', 1, "a"));
		BOOST_REQUIRE(vi.Insert('v', 2, "b"));
		BOOST_REQUIRE(vi.Insert('v', 3, "x"));
		BOOST_REQUIRE(layout::Generate(driver_.db(), "layout"));
		BOOST_REQUIRE_EQUAL(CreateTable(driver_.db(), "tacs", TACS_SCHEMA), 1);
		BOOST_REQUIRE_EQUAL(SaveNol(1, driver_.db()), 1);

		std::memset(&option_, 0, sizeof(option_));
	}

	~F()
	{
		boost::filesystem::remove("layout");
		boost::filesystem::remove("bc");
		boost::filesystem::remove("output");
	}

	void Insert(const char *name, int noir, int nod, const char *body)
	{
		db::TacInserter ti(driver_.db());
		BOOST_REQUIRE(ti.Insert(name, noir, nod, body));
	}

	job::Option option_;
};

#define SETUP(a, b) do {						\
		test::Sql sql(driver_.db());			\
		sql.Exec("DELETE FROM tacs");			\
		Insert("%a#0", 0, 1,					\
			   "  loadi $0 " #a "\n"			\
			   "  store %a#0 $0\n");			\
		Insert("%b#0", 0, 1,					\
			   "  loadi $0 " #b "\n"			\
			   "  store %b#0 $0\n");			\
	} while (0)

#define TESTCASE1(f, a, expected) do {									\
		SETUP(a, 0);													\
		Insert("%x#0", 0, 2,											\
			   "  load $1 %a#0\n"										\
			   "  $0 = (" #f " $1)\n"									\
			   "  store %x#0 $0\n");									\
		std::ofstream ofs("bc", std::ios::out|std::ios::binary);		\
		BOOST_REQUIRE(compiler::bcc::Bcc(driver_.db(), &ofs));			\
		ofs.close();													\
		FILE *fp = std::fopen("output", "wb");							\
		BOOST_REQUIRE(fp);												\
		option_.layout_file = "layout";									\
		option_.output_fp = fp;											\
		BOOST_REQUIRE(job::Evolve(driver_.db(), "bc", option_));		\
		std::fclose(fp);												\
		CheckOutput(expected);											\
	} while (0)

#define TESTCASE2(f, a, b, expected) do {								\
		SETUP(a, b);													\
		Insert("%x#0", 0, 3,											\
			   "  load $1 %a#0\n"										\
			   "  load $2 %b#0\n"										\
			   "  $0 = (" #f " $1 $2)\n"								\
			   "  store %x#0 $0\n");									\
		std::ofstream ofs("bc", std::ios::out|std::ios::binary);		\
		BOOST_REQUIRE(compiler::bcc::Bcc(driver_.db(), &ofs));			\
		ofs.close();													\
		FILE *fp = std::fopen("output", "wb");							\
		BOOST_REQUIRE(fp);												\
		option_.layout_file = "layout";									\
		option_.output_fp = fp;											\
		BOOST_REQUIRE(job::Evolve(driver_.db(), "bc", option_));		\
		std::fclose(fp);												\
		CheckOutput(expected);											\
	} while (0)

BOOST_FIXTURE_TEST_SUITE(test_evolve, F)

#include "common/runtime/call-testcases.h"

BOOST_AUTO_TEST_SUITE_END()
