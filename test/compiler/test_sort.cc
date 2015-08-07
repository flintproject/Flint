/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/sort.h"

#define BOOST_TEST_MODULE test_sort
#include "test.hh"

struct F : public test::MemoryFixture {
	F()
		: db(driver_.db())
		, sql(db)
	{
		sql.Exec("CREATE TABLE asts (uuid BLOB, name TEXT, math TEXT)");
	}

	// Generated from the following input:
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq (diff (bvar %time) %y) (divide (plus %v %w) pi))
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq %w (power exponentiale %v))
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq (diff (bvar %time) %x) (minus %y))
	// 6ac35158-41ed-11e3-bb0b-53c9de954475 (eq %v (plus %x %y))
	void SetupEuler()
	{
		sql.Exec("INSERT INTO asts VALUES (X'6ac3515841ed11e3bb0b53c9de954475', '%y#0', '(plus %y (times @dt (divide (plus %v %w) pi)))')");
		sql.Exec("INSERT INTO asts VALUES (X'6ac3515841ed11e3bb0b53c9de954475', '%w#0', '(power exponentiale %v#0)')");
		sql.Exec("INSERT INTO asts VALUES (X'6ac3515841ed11e3bb0b53c9de954475', '%x#0', '(plus %x (times @dt (minus %y)))')");
		sql.Exec("INSERT INTO asts VALUES (X'6ac3515841ed11e3bb0b53c9de954475', '%v#0', '(plus %x#0 %y#0)')");
	}

	// Generated from the following input:
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq %x %Y)
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq %y (divide %x 2.71828182845904523536028747135266249775724709369995))
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq %z ($DeltaTime %y))
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq (diff (bvar %time) %X) (minus %Y))
	// 4017c8b0-41ef-11e3-aec2-6f7051ba7a7a (eq (diff (bvar %time) %Y) (times %X 2))
	void SetupRk4()
	{
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%x#2', '%Y#2')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%x#4', '%Y#4')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%x#6', '%Y#6')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%x#0', '%Y#0')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%y#2', '(divide %x#2 2.71828182845904523536028747135266249775724709369995)')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%y#4', '(divide %x#4 2.71828182845904523536028747135266249775724709369995)')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%y#6', '(divide %x#6 2.71828182845904523536028747135266249775724709369995)')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%y#0', '(divide %x#0 2.71828182845904523536028747135266249775724709369995)')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%z#2', '($lookback %y#2 (minus (plus %time (divide @dt 2)) @dt))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%z#4', '($lookback %y#4 (minus (plus %time (divide @dt 2)) @dt))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%z#6', '($lookback %y#6 (minus (plus %time @dt) @dt))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%z#0', '($lookback %y#0 (minus (plus %time @dt) @dt))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#1', '(times @dt (minus %Y))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#2', '(plus %X (divide %X#1 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#3', '(times @dt (minus %Y#2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#4', '(plus %X (divide %X#3 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#5', '(times @dt (minus %Y#4))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#6', '(plus %X %X#5)')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#7', '(times @dt (minus %Y#6))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%X#0', '(plus %X (divide (plus %X#1 (plus (times 2 %X#3) (plus (times 2 %X#5) %X#7))) 6))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#1', '(times @dt (times %X 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#2', '(plus %Y (divide %Y#1 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#3', '(times @dt (times %X#2 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#4', '(plus %Y (divide %Y#3 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#5', '(times @dt (times %X#4 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#6', '(plus %Y %Y#5)')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#7', '(times @dt (times %X#6 2))')");
		sql.Exec("INSERT INTO asts VALUES (X'4017c8b041ef11e3aec26f7051ba7a7a', '%Y#0', '(plus %Y (divide (plus %Y#1 (plus (times 2 %Y#3) (plus (times 2 %Y#5) %Y#7))) 6))')");
	}

	// Generated from the following input:
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq (diff (bvar %time) %sbml:y) (divide (plus %sbml:v %sbml:w) pi))
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq %sbml:w (power exponentiale %sbml:v))
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq (diff (bvar %time) %sbml:x) (minus %sbml:y))
	// d1b5df78-4762-11e3-ba82-f38d65ddd815 (eq %sbml:v (plus %sbml:x %sbml:y))
	void SetupSbml()
	{
		sql.Exec("INSERT INTO asts VALUES (X'd1b5df78476211e3ba82f38d65ddd815', '%sbml:y#0', '(plus %sbml:y (times @dt (divide (plus %sbml:v %sbml:w) pi)))')");
		sql.Exec("INSERT INTO asts VALUES (X'd1b5df78476211e3ba82f38d65ddd815', '%sbml:w#0', '(power exponentiale %sbml:v#0)')");
		sql.Exec("INSERT INTO asts VALUES (X'd1b5df78476211e3ba82f38d65ddd815', '%sbml:x#0', '(plus %sbml:x (times @dt (minus %sbml:y)))')");
		sql.Exec("INSERT INTO asts VALUES (X'd1b5df78476211e3ba82f38d65ddd815', '%sbml:v#0', '(plus %sbml:x#0 %sbml:y#0)')");
	}

	void CheckSorts(const std::vector<std::string> &rows)
	{
		sql.CheckRows("SELECT lower(hex(uuid)), name, math FROM sorts", rows);
	}

	sqlite3 *db;
	test::Sql sql;
};

BOOST_FIXTURE_TEST_SUITE(test_sort, F)

BOOST_AUTO_TEST_CASE(Empty) {
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> rows;
	CheckSorts(rows);
}

BOOST_AUTO_TEST_CASE(Euler) {
	SetupEuler();
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> rows{
		"6ac3515841ed11e3bb0b53c9de954475 %y#0 (plus %y (times @dt (divide (plus %v %w) pi)))",
		"6ac3515841ed11e3bb0b53c9de954475 %x#0 (plus %x (times @dt (minus %y)))",
		"6ac3515841ed11e3bb0b53c9de954475 %v#0 (plus %x#0 %y#0)",
		"6ac3515841ed11e3bb0b53c9de954475 %w#0 (power exponentiale %v#0)"
	};
	CheckSorts(rows);
}

BOOST_AUTO_TEST_CASE(Rk4) {
	SetupRk4();
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> rows{
		"4017c8b041ef11e3aec26f7051ba7a7a %X#1 (times @dt (minus %Y))",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#1 (times @dt (times %X 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %X#2 (plus %X (divide %X#1 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#2 (plus %Y (divide %Y#1 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %x#2 %Y#2",
		"4017c8b041ef11e3aec26f7051ba7a7a %X#3 (times @dt (minus %Y#2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#3 (times @dt (times %X#2 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %y#2 (divide %x#2 2.71828182845904523536028747135266249775724709369995)",
		"4017c8b041ef11e3aec26f7051ba7a7a %X#4 (plus %X (divide %X#3 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#4 (plus %Y (divide %Y#3 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %x#4 %Y#4",
		"4017c8b041ef11e3aec26f7051ba7a7a %z#2 ($lookback %y#2 (minus (plus %time (divide @dt 2)) @dt))",
		"4017c8b041ef11e3aec26f7051ba7a7a %X#5 (times @dt (minus %Y#4))",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#5 (times @dt (times %X#4 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %y#4 (divide %x#4 2.71828182845904523536028747135266249775724709369995)",
		"4017c8b041ef11e3aec26f7051ba7a7a %X#6 (plus %X %X#5)",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#6 (plus %Y %Y#5)",
		"4017c8b041ef11e3aec26f7051ba7a7a %x#6 %Y#6",
		"4017c8b041ef11e3aec26f7051ba7a7a %z#4 ($lookback %y#4 (minus (plus %time (divide @dt 2)) @dt))",
		"4017c8b041ef11e3aec26f7051ba7a7a %X#7 (times @dt (minus %Y#6))",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#7 (times @dt (times %X#6 2))",
		"4017c8b041ef11e3aec26f7051ba7a7a %y#6 (divide %x#6 2.71828182845904523536028747135266249775724709369995)",
		"4017c8b041ef11e3aec26f7051ba7a7a %X#0 (plus %X (divide (plus %X#1 (plus (times 2 %X#3) (plus (times 2 %X#5) %X#7))) 6))",
		"4017c8b041ef11e3aec26f7051ba7a7a %Y#0 (plus %Y (divide (plus %Y#1 (plus (times 2 %Y#3) (plus (times 2 %Y#5) %Y#7))) 6))",
		"4017c8b041ef11e3aec26f7051ba7a7a %x#0 %Y#0",
		"4017c8b041ef11e3aec26f7051ba7a7a %z#6 ($lookback %y#6 (minus (plus %time @dt) @dt))",
		"4017c8b041ef11e3aec26f7051ba7a7a %y#0 (divide %x#0 2.71828182845904523536028747135266249775724709369995)",
		"4017c8b041ef11e3aec26f7051ba7a7a %z#0 ($lookback %y#0 (minus (plus %time @dt) @dt))",
	};
	CheckSorts(rows);
}

BOOST_AUTO_TEST_CASE(Sbml) {
	SetupSbml();
	BOOST_CHECK(compiler::sort::Sort(db));
	std::vector<std::string> rows{
		"d1b5df78476211e3ba82f38d65ddd815 %sbml:y#0 (plus %sbml:y (times @dt (divide (plus %sbml:v %sbml:w) pi)))",
		"d1b5df78476211e3ba82f38d65ddd815 %sbml:x#0 (plus %sbml:x (times @dt (minus %sbml:y)))",
		"d1b5df78476211e3ba82f38d65ddd815 %sbml:v#0 (plus %sbml:x#0 %sbml:y#0)",
		"d1b5df78476211e3ba82f38d65ddd815 %sbml:w#0 (power exponentiale %sbml:v#0)"
	};
	CheckSorts(rows);
}

BOOST_AUTO_TEST_SUITE_END()
