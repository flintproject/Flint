/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.h"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "bc/index.h"
#include "cas.h"
#include "compiler/bcc.h"
#include "database.h"
#include "db/helper.h"
#include "db/query.h"
#include "db/tac-inserter.h"
#include "db/variable-inserter.h"
#include "filter/writer.h"
#include "flint/bc.h"
#include "flint/ls.h"
#include "layout.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "task.h"

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
		db::VariableInserter vi("variables", false, driver_.db());
		BOOST_REQUIRE(vi.Insert('v', 1, "a"));
		BOOST_REQUIRE(vi.Insert('v', 2, "b"));
		BOOST_REQUIRE(vi.Insert('v', 3, "x"));
		BOOST_REQUIRE(layout::Generate(driver_.db(), "layout"));
		BOOST_REQUIRE_EQUAL(CreateTable(driver_.db(), "tacs", TACS_SCHEMA), 1);
		BOOST_REQUIRE_EQUAL(SaveNol(1, driver_.db()), 1);

		option_.id = 0;
		option_.input_data = nullptr;
		option_.fppp_option = nullptr;
		option_.arg = nullptr;

		task_.method = compiler::Method::kEuler;
		task_.length = task_.step = 0.01;
		task_.granularity = 1;
		task_.output_start_time = 0;
		std::unique_ptr<Layout> layout(new Layout);
		LayoutLoader loader("layout");
		BOOST_REQUIRE(loader.Load(layout.get()));
		size_t layer_size = layout->Calculate();
		task_.layout.swap(layout);
		task_.layer_size = layer_size;
	}

	~F()
	{
		boost::filesystem::remove("layout");
		boost::filesystem::remove("output");
	}

	void Insert(const char *name, int noir, int nod, const char *body)
	{
		db::TacInserter ti(driver_.db());
		BOOST_REQUIRE(ti.Insert(name, noir, nod, body));
	}

	job::Option option_;
	task::Task task_;
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
		task_.bc.reset(compiler::bcc::Bcc(driver_.db()));				\
		BOOST_REQUIRE(task_.bc);										\
		std::ofstream ofs("output", std::ios::out|std::ios::binary);	\
		BOOST_REQUIRE(ofs);												\
		option_.output_stream = &ofs;									\
		BOOST_REQUIRE(job::Evolve(task_, option_) == job::Result::kSucceeded); \
		ofs.close();													\
		CheckOutput(expected);											\
	} while (0)

#define TESTCASE2(f, a, b, expected) do {								\
		SETUP(a, b);													\
		Insert("%x#0", 0, 3,											\
			   "  load $1 %a#0\n"										\
			   "  load $2 %b#0\n"										\
			   "  $0 = (" #f " $1 $2)\n"								\
			   "  store %x#0 $0\n");									\
		task_.bc.reset(compiler::bcc::Bcc(driver_.db()));				\
		std::ofstream ofs("output", std::ios::out|std::ios::binary);	\
		BOOST_REQUIRE(ofs);												\
		option_.output_stream = &ofs;									\
		BOOST_REQUIRE(job::Evolve(task_, option_) == job::Result::kSucceeded); \
		ofs.close();													\
		CheckOutput(expected);											\
	} while (0)

BOOST_FIXTURE_TEST_SUITE(test_evolve, F)

#include "common/runtime/call-testcases.h"

BOOST_AUTO_TEST_SUITE_END()
