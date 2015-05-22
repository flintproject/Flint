/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEST_METHOD_F_HH_
#define FLINT_TEST_METHOD_F_HH_

#include "db/driver.h"

struct F {
	F()
		: driver("")
		, db(driver.db())
		, sql(db)
	{
		sql.Exec("CREATE TABLE input (uuid TEXT, math TEXT)");
		sql.Exec("CREATE TABLE output (uuid TEXT, name TEXT, math TEXT)");
	}

	~F() {
	}

	void SetupConditional()
	{
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(case-set (case (condition (eq %x %y)) (eq %a (plus 1 pi))))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(case-set (case (condition (leq %x %y)) (eq %b 0)) (case (eq %b 1)))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(case-set (case (condition (eq %x %y)) (eq %c 0)) (case (condition (lt %x %y)) (eq %c 1)))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(case-set (case (condition (eq %x %y)) (eq %d 0)) (case (condition (gt %x %y)) (eq %d 1)) (case (eq %d 2)))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(case-set (case (condition (and (geq %time %a) (lt %time (plus %a 5)))) (eq %x (plus %y 1))) (case (eq %x 0)))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(case-set (case (condition (eq %time 0.1)) (eq %y %b)) (case (condition (neq %time 0.2)) (eq %y %c)))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(case-set (case (condition (lt %x 1)) (eq %z 0)) (case (condition (lt %x 2)) (case-set (case (condition (lt %y 1)) (eq %z 1)) (case (condition (lt %y 2)) (eq %z 2)) (case (eq %z 3)))) (case (case-set (case (condition (lt %y 3)) (eq %z 4)) (case (eq %z 5)))))')");
	}

	void SetupFunction()
	{
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %x ($exponential_variate 10))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %y ($gamma_variate 2 1))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %z ($gauss_variate 0 1))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %x0 ($poisson_variate 10))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %x1 ($uniform_variate 0 100 0 1))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %v ($Delay %x 0.001))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %w ($DeltaTime %x))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %t2 (times %time 2))')");
	}

	void SetupLiteral()
	{
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %x 1.2e-3)')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %y 45E-06)')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %z (times -2.3e+30 .5E-10))')");

	}

	void SetupOde()
	{
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq (diff (bvar %time) %x) (minus %y))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq (diff (bvar %time) %y) (times %x 2))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq (diff (bvar %time) %z) (plus %time %x))')");

	}

	void SetupSbml()
	{
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %sbml:y (power %sbml:x 2))')");
		sql.Exec("INSERT INTO input VALUES ('00000000-0000-0000-0000-000000000000', '(eq %sbml:z (times %sbml:y 0.5))')");
	}

	db::Driver driver;
	sqlite3 *db;
	test::Sql sql;
};

#endif
