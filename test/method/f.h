/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEST_METHOD_F_H_
#define FLINT_TEST_METHOD_F_H_

#include "db/driver.h"

struct F {
	F()
		: input_driver(":memory:")
		, input_db(input_driver.db())
		, input(input_db)
		, output_driver(":memory:")
		, output_db(output_driver.db())
		, output(output_db)
	{
		input.Exec("CREATE TABLE input (uuid BLOB, math TEXT)");
	}

	~F() {
	}

	void SetupConditional()
	{
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(case-set (case (condition (eq %x %y)) (eq %a (plus 1 pi))))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(case-set (case (condition (leq %x %y)) (eq %b 0)) (case (eq %b 1)))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(case-set (case (condition (eq %x %y)) (eq %c 0)) (case (condition (lt %x %y)) (eq %c 1)))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(case-set (case (condition (eq %x %y)) (eq %d 0)) (case (condition (gt %x %y)) (eq %d 1)) (case (eq %d 2)))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(case-set (case (condition (and (geq %time %a) (lt %time (plus %a 5)))) (eq %x (plus %y 1))) (case (eq %x 0)))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(case-set (case (condition (eq %time 0.1)) (eq %y %b)) (case (condition (neq %time 0.2)) (eq %y %c)))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(case-set (case (condition (lt %x 1)) (eq %z 0)) (case (condition (lt %x 2)) (case-set (case (condition (lt %y 1)) (eq %z 1)) (case (condition (lt %y 2)) (eq %z 2)) (case (eq %z 3)))) (case (case-set (case (condition (lt %y 3)) (eq %z 4)) (case (eq %z 5)))))')");
	}

	void SetupFunction()
	{
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %x ($exponential_variate 10))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %y ($gamma_variate 2 1))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %z ($gauss_variate 0 1))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %lnv ($lognormal_variate 0 1))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %x0 ($poisson_variate 10))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %x1 ($uniform_variate 0 100 0 1))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %wv ($weibull_variate 1 0.5))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %v ($Delay %x 0.001))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %w ($DeltaTime %x))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %last_time ($DeltaTime %time))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %t2 (times %time 2))')");
	}

	void SetupLiteral()
	{
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %e 2.71828182845904523536028747135266249775724709369995)')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %x 1.2e-3)')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %y 45E-06)')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %z (times -2.3e+30 .5E-10))')");
	}

	void SetupOde()
	{
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq (diff (bvar %time) %x) (minus %y))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq (diff (bvar %time) %y) (times %x 2))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq (diff (bvar %time) %z) (plus %time %x))')");

	}

	void SetupSbml()
	{
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %sbml:y (power %sbml:x 2))')");
		input.Exec("INSERT INTO input VALUES (X'00000000000000000000000000000000', '(eq %sbml:z (times %sbml:y 0.5))')");
	}

	void CheckAsts(const std::vector<std::string> &rows)
	{
		output.CheckRows("SELECT hex(uuid), name, math FROM asts", rows);
	}

	db::Driver input_driver;
	sqlite3 *input_db;
	test::Sql input;
	db::Driver output_driver;
	sqlite3 *output_db;
	test::Sql output;
};

#endif
