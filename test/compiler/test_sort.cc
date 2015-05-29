/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/sort.h"

#define BOOST_TEST_MODULE test_sort
#include "test.hh"

#include "db/driver.h"

struct F {
	F()
		: driver("")
		, db(driver.db())
		, sql(db)
	{
		sql.Exec("CREATE TABLE asts (uuid TEXT, name TEXT, math TEXT)");
	}

	// Generated from the following input:
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq (diff (bvar %time) %y) (divide (plus %v %w) pi))
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq %w (power exponentiale %v))
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq (diff (bvar %time) %x) (minus %y))
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq %v (plus %x %y))
	void SetupEuler()
	{
		sql.Exec("INSERT INTO asts VALUES ('6ac35158-41ed-11e3-bb0b-53c9de954475', '%y#0', '(plus %y (times @dt (divide (plus %v %w) pi)))')");
		sql.Exec("INSERT INTO asts VALUES ('6ac35158-41ed-11e3-bb0b-53c9de954475', '%w#0', '(power exponentiale %v#0)')");
		sql.Exec("INSERT INTO asts VALUES ('6ac35158-41ed-11e3-bb0b-53c9de954475', '%x#0', '(plus %x (times @dt (minus %y)))')");
		sql.Exec("INSERT INTO asts VALUES ('6ac35158-41ed-11e3-bb0b-53c9de954475', '%v#0', '(plus %x#0 %y#0)')");
	}

	// Generated from the following input:
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq %x %Y)
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq %y (divide %x 2))
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq %z ($DeltaTime %y))
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq (diff (bvar %time) %X) (minus %Y))
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq (diff (bvar %time) %Y) (times %X 2))
	void SetupRk4()
	{
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%x#2', '%Y#2')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%x#4', '%Y#4')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%x#6', '%Y#6')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%x#0', '%Y#0')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%y#2', '(divide %x#2 2)')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%y#4', '(divide %x#4 2)')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%y#6', '(divide %x#6 2)')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%y#0', '(divide %x#0 2)')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%z#2', '($lookback %y#2 (minus (plus %time (divide @dt 2)) @dt))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%z#4', '($lookback %y#4 (minus (plus %time (divide @dt 2)) @dt))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%z#6', '($lookback %y#6 (minus (plus %time @dt) @dt))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%z#0', '($lookback %y#0 (minus (plus %time @dt) @dt))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#1', '(times @dt (minus %Y))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#2', '(plus %X (divide %X#1 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#3', '(times @dt (minus %Y#2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#4', '(plus %X (divide %X#3 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#5', '(times @dt (minus %Y#4))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#6', '(plus %X %X#5)')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#7', '(times @dt (minus %Y#6))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%X#0', '(plus %X (divide (plus %X#1 (plus (times 2 %X#3) (plus (times 2 %X#5) %X#7))) 6))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#1', '(times @dt (times %X 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#2', '(plus %Y (divide %Y#1 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#3', '(times @dt (times %X#2 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#4', '(plus %Y (divide %Y#3 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#5', '(times @dt (times %X#4 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#6', '(plus %Y %Y#5)')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#7', '(times @dt (times %X#6 2))')");
		sql.Exec("INSERT INTO asts VALUES ('4017c8b0-41ef-11e3-aec2-6f7051ba7a7a', '%Y#0', '(plus %Y (divide (plus %Y#1 (plus (times 2 %Y#3) (plus (times 2 %Y#5) %Y#7))) 6))')");
	}

	// Generated from the following input:
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq (diff (bvar %time) %sbml:y) (divide (plus %sbml:v %sbml:w) pi))
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq %sbml:w (power exponentiale %sbml:v))
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq (diff (bvar %time) %sbml:x) (minus %sbml:y))
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq %sbml:v (plus %sbml:x %sbml:y))
	void SetupSbml()
	{
		sql.Exec("INSERT INTO asts VALUES ('d1b5df78-4762-11e3-ba82-f38d65ddd815', '%sbml:y#0', '(plus %sbml:y (times @dt (divide (plus %sbml:v %sbml:w) pi)))')");
		sql.Exec("INSERT INTO asts VALUES ('d1b5df78-4762-11e3-ba82-f38d65ddd815', '%sbml:w#0', '(power exponentiale %sbml:v#0)')");
		sql.Exec("INSERT INTO asts VALUES ('d1b5df78-4762-11e3-ba82-f38d65ddd815', '%sbml:x#0', '(plus %sbml:x (times @dt (minus %sbml:y)))')");
		sql.Exec("INSERT INTO asts VALUES ('d1b5df78-4762-11e3-ba82-f38d65ddd815', '%sbml:v#0', '(plus %sbml:x#0 %sbml:y#0)')");
	}

	db::Driver driver;
	sqlite3 *db;
	test::Sql sql;
};

BOOST_FIXTURE_TEST_SUITE(test_sort, F)

BOOST_AUTO_TEST_CASE(Empty) {
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> r;
	sql.Table("sorts", &r);
	BOOST_CHECK(r.empty());
}

BOOST_AUTO_TEST_CASE(Euler) {
	SetupEuler();
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> r;
	sql.Table("sorts", &r);
	BOOST_CHECK_EQUAL(r.size(), 4u);
	BOOST_CHECK_EQUAL(r[0], "6ac35158-41ed-11e3-bb0b-53c9de954475 %y#0 (plus %y (times @dt (divide (plus %v %w) pi)))");
	BOOST_CHECK_EQUAL(r[1], "6ac35158-41ed-11e3-bb0b-53c9de954475 %x#0 (plus %x (times @dt (minus %y)))");
	BOOST_CHECK_EQUAL(r[2], "6ac35158-41ed-11e3-bb0b-53c9de954475 %v#0 (plus %x#0 %y#0)");
	BOOST_CHECK_EQUAL(r[3], "6ac35158-41ed-11e3-bb0b-53c9de954475 %w#0 (power exponentiale %v#0)");
}

BOOST_AUTO_TEST_CASE(Rk4) {
	SetupRk4();
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> r;
	sql.Table("sorts", &r);
	BOOST_CHECK_EQUAL(r.size(), 28u);
	BOOST_CHECK_EQUAL(r[0], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#1 (times @dt (minus %Y))");
	BOOST_CHECK_EQUAL(r[1], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#1 (times @dt (times %X 2))");
	BOOST_CHECK_EQUAL(r[2], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#2 (plus %X (divide %X#1 2))");
	BOOST_CHECK_EQUAL(r[3], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#2 (plus %Y (divide %Y#1 2))");
	BOOST_CHECK_EQUAL(r[4], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#3 (times @dt (times %X#2 2))");
	BOOST_CHECK_EQUAL(r[5], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#4 (plus %Y (divide %Y#3 2))");
	BOOST_CHECK_EQUAL(r[6], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %x#2 %Y#2");
	BOOST_CHECK_EQUAL(r[7], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %x#4 %Y#4");
	BOOST_CHECK_EQUAL(r[8], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %y#2 (divide %x#2 2)");
	BOOST_CHECK_EQUAL(r[9], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %y#4 (divide %x#4 2)");
	BOOST_CHECK_EQUAL(r[10], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %z#2 ($lookback %y#2 (minus (plus %time (divide @dt 2)) @dt))");
	BOOST_CHECK_EQUAL(r[11], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %z#4 ($lookback %y#4 (minus (plus %time (divide @dt 2)) @dt))");
	BOOST_CHECK_EQUAL(r[12], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#3 (times @dt (minus %Y#2))");
	BOOST_CHECK_EQUAL(r[13], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#4 (plus %X (divide %X#3 2))");
	BOOST_CHECK_EQUAL(r[14], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#5 (times @dt (minus %Y#4))");
	BOOST_CHECK_EQUAL(r[15], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#6 (plus %X %X#5)");
	BOOST_CHECK_EQUAL(r[16], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#5 (times @dt (times %X#4 2))");
	BOOST_CHECK_EQUAL(r[17], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#6 (plus %Y %Y#5)");
	BOOST_CHECK_EQUAL(r[18], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#7 (times @dt (times %X#6 2))");
	BOOST_CHECK_EQUAL(r[19], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %Y#0 (plus %Y (divide (plus %Y#1 (plus (times 2 %Y#3) (plus (times 2 %Y#5) %Y#7))) 6))");
	BOOST_CHECK_EQUAL(r[20], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %x#6 %Y#6");
	BOOST_CHECK_EQUAL(r[21], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %x#0 %Y#0");
	BOOST_CHECK_EQUAL(r[22], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %y#6 (divide %x#6 2)");
	BOOST_CHECK_EQUAL(r[23], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %y#0 (divide %x#0 2)");
	BOOST_CHECK_EQUAL(r[24], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %z#6 ($lookback %y#6 (minus (plus %time @dt) @dt))");
	BOOST_CHECK_EQUAL(r[25], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %z#0 ($lookback %y#0 (minus (plus %time @dt) @dt))");
	BOOST_CHECK_EQUAL(r[26], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#7 (times @dt (minus %Y#6))");
	BOOST_CHECK_EQUAL(r[27], "4017c8b0-41ef-11e3-aec2-6f7051ba7a7a %X#0 (plus %X (divide (plus %X#1 (plus (times 2 %X#3) (plus (times 2 %X#5) %X#7))) 6))");
}

BOOST_AUTO_TEST_CASE(Sbml) {
	SetupSbml();
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> r;
	sql.Table("sorts", &r);
	BOOST_CHECK_EQUAL(r.size(), 4u);
	BOOST_CHECK_EQUAL(r[0], "d1b5df78-4762-11e3-ba82-f38d65ddd815 %sbml:y#0 (plus %sbml:y (times @dt (divide (plus %sbml:v %sbml:w) pi)))");
	BOOST_CHECK_EQUAL(r[1], "d1b5df78-4762-11e3-ba82-f38d65ddd815 %sbml:x#0 (plus %sbml:x (times @dt (minus %sbml:y)))");
	BOOST_CHECK_EQUAL(r[2], "d1b5df78-4762-11e3-ba82-f38d65ddd815 %sbml:v#0 (plus %sbml:x#0 %sbml:y#0)");
	BOOST_CHECK_EQUAL(r[3], "d1b5df78-4762-11e3-ba82-f38d65ddd815 %sbml:w#0 (power exponentiale %sbml:v#0)");
}

BOOST_AUTO_TEST_SUITE_END()