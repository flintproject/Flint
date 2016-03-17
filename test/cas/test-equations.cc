/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas.h"

#include "db/helper.h"

#define BOOST_TEST_MODULE test_equations
#include "test.h"

const char *kUnaryFunctions[] = {
	"abs",
	"arccos",
	"arccosh",
	"arccot",
	"arccoth",
	"arccsc",
	"arccsch",
	"arcsec",
	"arcsech",
	"arcsin",
	"arcsinh",
	"arctan",
	"arctanh",
	"ceiling",
	"cos",
	"cosh",
	"cot",
	"coth",
	"csc",
	"csch",
	"exp",
	"floor",
	"ln",
	"log",
	"minus",
	"plus",
	"root",
	"sec",
	"sech",
	"sin",
	"sinh",
	"tan",
	"tanh",
};

const char *kBinaryFunctions[] = {
	"divide",
	"eq",
	"geq",
	"gt",
	"leq",
	"log",
	"lt",
	"max",
	"min",
	"minus",
	"neq",
	"plus",
	"power",
	"rem",
	"root",
	"times",
};

const char *kNaryFunctions[] = {
	"mean"
};

const int kNumUnaryFunctions = static_cast<int>(sizeof(kUnaryFunctions)/sizeof(kUnaryFunctions[0]));
const int kNumBinaryFunctions = static_cast<int>(sizeof(kBinaryFunctions)/sizeof(kBinaryFunctions[0]));
const int kNumNaryFunctions = static_cast<int>(sizeof(kNaryFunctions)/sizeof(kNaryFunctions[0]));

struct F : public test::MemoryFixture {
	F()
		: db(driver_.db())
		, sql(db)
		, system(new cas::System)
	{
		sql.Exec("CREATE TABLE variables " VARIABLES_SCHEMA);
		AddVariable('v', 1, "v", 1, 1);
		AddVariable('v', 2, "w", 1, 1);
		AddVariable('x', 3, "x", 1, 1);
		AddVariable('x', 4, "y", 1, 1);
		AddVariable('x', 5, "z", 1, 3);

		char name[8];
		for (int i=0;i<kNumUnaryFunctions+kNumNaryFunctions;i++) {
			int id = i+1000;
			std::sprintf(name, "x%d", id);
			AddVariable('v', id, name, 1, 1);
		}
		for (int i=0;i<kNumBinaryFunctions+kNumNaryFunctions;i++) {
			int id = i+2000;
			std::sprintf(name, "xy%d", id);
			AddVariable('v', id, name, 1, 1);
		}

		sql.Exec("CREATE TABLE input (uuid BLOB, math TEXT)");
		AddInput("(eq (diff (bvar %time) %y) (divide (plus %v %w) pi))");
		AddInput("(eq %w (power exponentiale %v))");
		AddInput("(eq (diff (bvar %time) %x) (minus %y))");
		AddInput("(eq %v (piecewise (piece (plus %x %y) (or (leq %time 1) (and (geq %x 0) (gt %y 0)))) (piece %x (lt %y 0)) (otherwise %y)))");
		AddInput("(eq (times (matrix (matrixrow 2 0 0) (matrixrow 0 -1.5 0) (matrixrow 0 0.5 0)) (diff (bvar %time) %z)) (vector %v %w %x))");

		char math[128];
		for (int i=0;i<kNumUnaryFunctions;i++) {
			std::sprintf(math, "(eq %%x%d (%s %%x))", i+1000, kUnaryFunctions[i]);
			AddInput(math);
		}
		for (int i=0;i<kNumBinaryFunctions;i++) {
			std::sprintf(math, "(eq %%xy%d (%s %%x %%y))", i+2000, kBinaryFunctions[i]);
			AddInput(math);
		}
		for (int i=0;i<kNumNaryFunctions;i++) {
			std::sprintf(math, "(eq %%x%d (%s %%x))", i+1000+kNumUnaryFunctions, kNaryFunctions[i]);
			AddInput(math);
			std::sprintf(math, "(eq %%xy%d (%s %%x %%y))", i+2000+kNumBinaryFunctions, kNaryFunctions[i]);
			AddInput(math);
		}

		system->Load(db);
	}

	void AddVariable(char type, int id, const char *name, int col, int row)
	{
		size_t s = std::strlen(name);
		std::unique_ptr<char[]> query(new char[s+128]);
		std::sprintf(query.get(),
					 "INSERT INTO variables VALUES (X'0a70b274940611e592a36be8bb430f97', '%c', '%d', '%s', 'dimensionless', '%d', '%d', NULL)",
					 type, id, name, col, row);
		sql.Exec(query.get());
	}

	void AddInput(const char *math)
	{
		size_t s = std::strlen(math);
		std::unique_ptr<char[]> query(new char[s+128]);
		std::sprintf(query.get(), "INSERT INTO input VALUES (X'0a70b274940611e592a36be8bb430f97', '%s')", math);
		sql.Exec(query.get());
	}

	sqlite3 *db;
	test::Sql sql;
	std::unique_ptr<cas::System> system;
};

BOOST_FIXTURE_TEST_SUITE(test_equations, F)

BOOST_AUTO_TEST_CASE(Test) {
	BOOST_CHECK(cas::AnnotateEquations(db, "input", system.get()));
}

BOOST_AUTO_TEST_SUITE_END()
