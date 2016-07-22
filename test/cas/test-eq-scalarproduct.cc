/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas.h"

#include "db/helper.h"

#define BOOST_TEST_MODULE test_eq_scalarproduct
#include "test.h"

struct F : public test::MemoryFixture {
	F()
		: db(driver_.db())
		, sql(db)
		, system(new cas::System)
	{
		sql.Exec("CREATE TABLE variables " VARIABLES_SCHEMA);
		AddVariable('v', 1, "x", 1, 1);
		AddVariable('v', 2, "y", 1, 1);
		AddVariable('v', 3, "a", 3, 1);
		AddVariable('s', 4, "b", 3, 1);
		AddVariable('v', 5, "c", 1, 4);
		AddVariable('x', 6, "d", 1, 4);

		sql.Exec("CREATE TABLE input (uuid BLOB, math TEXT)");
		AddInput("(eq %x (scalarproduct %a %b))");
		AddInput("(eq %y (scalarproduct %c %d))");

		system->Load(db);
	}

	void AddVariable(char type, int id, const char *name, int col, int row)
	{
		size_t s = std::strlen(name);
		std::unique_ptr<char[]> query(new char[s+128]);
		std::sprintf(query.get(),
					 "INSERT INTO variables VALUES (X'2118c3c6a54611e582d0a78cd2dbe5ee', '%c', '%d', '%s', 'dimensionless', '%d', '%d', NULL, '0')",
					 type, id, name, col, row);
		sql.Exec(query.get());
	}

	void AddInput(const char *math)
	{
		size_t s = std::strlen(math);
		std::unique_ptr<char[]> query(new char[s+128]);
		std::sprintf(query.get(), "INSERT INTO input VALUES (X'2118c3c6a54611e582d0a78cd2dbe5ee', '%s')", math);
		sql.Exec(query.get());
	}

	sqlite3 *db;
	test::Sql sql;
	std::unique_ptr<cas::System> system;
};

BOOST_FIXTURE_TEST_SUITE(test_eq_scalarproduct, F)

BOOST_AUTO_TEST_CASE(Test) {
	BOOST_CHECK(cas::AnnotateEquations(db, "input", system.get()));
}

BOOST_AUTO_TEST_SUITE_END()
