/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method.hh"

#define BOOST_TEST_MODULE test_rk4
#include "test.hh"

#include "f.hh"

BOOST_FIXTURE_TEST_SUITE(test_rk4, F)

BOOST_AUTO_TEST_CASE(empty) {
	BOOST_CHECK(method::Rk4(input_db, "input", output_db));
}

BOOST_AUTO_TEST_CASE(conditional) {
	SetupConditional();
	BOOST_CHECK(method::Rk4(input_db, "input", output_db));
	std::vector<std::string> r;
	output.Table("asts", &r);
	BOOST_CHECK_EQUAL(r.size(), 28u);
	BOOST_CHECK_EQUAL(r[0], "00000000-0000-0000-0000-000000000000 %a#2 (piecewise (piece (plus 1 pi) (eq %x#2 %y#2)))");
	BOOST_CHECK_EQUAL(r[1], "00000000-0000-0000-0000-000000000000 %a#4 (piecewise (piece (plus 1 pi) (eq %x#4 %y#4)))");
	BOOST_CHECK_EQUAL(r[2], "00000000-0000-0000-0000-000000000000 %a#6 (piecewise (piece (plus 1 pi) (eq %x#6 %y#6)))");
	BOOST_CHECK_EQUAL(r[3], "00000000-0000-0000-0000-000000000000 %a#0 (piecewise (piece (plus 1 pi) (eq %x#0 %y#0)))");
	BOOST_CHECK_EQUAL(r[4], "00000000-0000-0000-0000-000000000000 %b#2 (piecewise (piece 0 (leq %x#2 %y#2)) (otherwise 1))");
	BOOST_CHECK_EQUAL(r[5], "00000000-0000-0000-0000-000000000000 %b#4 (piecewise (piece 0 (leq %x#4 %y#4)) (otherwise 1))");
	BOOST_CHECK_EQUAL(r[6], "00000000-0000-0000-0000-000000000000 %b#6 (piecewise (piece 0 (leq %x#6 %y#6)) (otherwise 1))");
	BOOST_CHECK_EQUAL(r[7], "00000000-0000-0000-0000-000000000000 %b#0 (piecewise (piece 0 (leq %x#0 %y#0)) (otherwise 1))");
	BOOST_CHECK_EQUAL(r[8], "00000000-0000-0000-0000-000000000000 %c#2 (piecewise (piece 0 (eq %x#2 %y#2)) (piece 1 (lt %x#2 %y#2)))");
	BOOST_CHECK_EQUAL(r[9], "00000000-0000-0000-0000-000000000000 %c#4 (piecewise (piece 0 (eq %x#4 %y#4)) (piece 1 (lt %x#4 %y#4)))");
	BOOST_CHECK_EQUAL(r[10], "00000000-0000-0000-0000-000000000000 %c#6 (piecewise (piece 0 (eq %x#6 %y#6)) (piece 1 (lt %x#6 %y#6)))");
	BOOST_CHECK_EQUAL(r[11], "00000000-0000-0000-0000-000000000000 %c#0 (piecewise (piece 0 (eq %x#0 %y#0)) (piece 1 (lt %x#0 %y#0)))");
	BOOST_CHECK_EQUAL(r[12], "00000000-0000-0000-0000-000000000000 %d#2 (piecewise (piece 0 (eq %x#2 %y#2)) (piece 1 (gt %x#2 %y#2)) (otherwise 2))");
	BOOST_CHECK_EQUAL(r[13], "00000000-0000-0000-0000-000000000000 %d#4 (piecewise (piece 0 (eq %x#4 %y#4)) (piece 1 (gt %x#4 %y#4)) (otherwise 2))");
	BOOST_CHECK_EQUAL(r[14], "00000000-0000-0000-0000-000000000000 %d#6 (piecewise (piece 0 (eq %x#6 %y#6)) (piece 1 (gt %x#6 %y#6)) (otherwise 2))");
	BOOST_CHECK_EQUAL(r[15], "00000000-0000-0000-0000-000000000000 %d#0 (piecewise (piece 0 (eq %x#0 %y#0)) (piece 1 (gt %x#0 %y#0)) (otherwise 2))");
	BOOST_CHECK_EQUAL(r[16], "00000000-0000-0000-0000-000000000000 %x#2 (piecewise (piece (plus %y#2 1) (and (geq (plus %time (divide @dt 2)) %a#2) (lt (plus %time (divide @dt 2)) (plus %a#2 5)))) (otherwise 0))");
	BOOST_CHECK_EQUAL(r[17], "00000000-0000-0000-0000-000000000000 %x#4 (piecewise (piece (plus %y#4 1) (and (geq (plus %time (divide @dt 2)) %a#4) (lt (plus %time (divide @dt 2)) (plus %a#4 5)))) (otherwise 0))");
	BOOST_CHECK_EQUAL(r[18], "00000000-0000-0000-0000-000000000000 %x#6 (piecewise (piece (plus %y#6 1) (and (geq (plus %time @dt) %a#6) (lt (plus %time @dt) (plus %a#6 5)))) (otherwise 0))");
	BOOST_CHECK_EQUAL(r[19], "00000000-0000-0000-0000-000000000000 %x#0 (piecewise (piece (plus %y#0 1) (and (geq (plus %time @dt) %a#0) (lt (plus %time @dt) (plus %a#0 5)))) (otherwise 0))");
	BOOST_CHECK_EQUAL(r[20], "00000000-0000-0000-0000-000000000000 %y#2 (piecewise (piece %b#2 (eq (plus %time (divide @dt 2)) 0.1)) (piece %c#2 (neq (plus %time (divide @dt 2)) 0.2)))");
	BOOST_CHECK_EQUAL(r[21], "00000000-0000-0000-0000-000000000000 %y#4 (piecewise (piece %b#4 (eq (plus %time (divide @dt 2)) 0.1)) (piece %c#4 (neq (plus %time (divide @dt 2)) 0.2)))");
	BOOST_CHECK_EQUAL(r[22], "00000000-0000-0000-0000-000000000000 %y#6 (piecewise (piece %b#6 (eq (plus %time @dt) 0.1)) (piece %c#6 (neq (plus %time @dt) 0.2)))");
	BOOST_CHECK_EQUAL(r[23], "00000000-0000-0000-0000-000000000000 %y#0 (piecewise (piece %b#0 (eq (plus %time @dt) 0.1)) (piece %c#0 (neq (plus %time @dt) 0.2)))");
	BOOST_CHECK_EQUAL(r[24], "00000000-0000-0000-0000-000000000000 %z#2 (piecewise (piece 0 (lt %x#2 1)) (piece (piecewise (piece 1 (lt %y#2 1)) (piece 2 (lt %y#2 2)) (otherwise 3)) (lt %x#2 2)) (otherwise (piecewise (piece 4 (lt %y#2 3)) (otherwise 5))))");
	BOOST_CHECK_EQUAL(r[25], "00000000-0000-0000-0000-000000000000 %z#4 (piecewise (piece 0 (lt %x#4 1)) (piece (piecewise (piece 1 (lt %y#4 1)) (piece 2 (lt %y#4 2)) (otherwise 3)) (lt %x#4 2)) (otherwise (piecewise (piece 4 (lt %y#4 3)) (otherwise 5))))");
	BOOST_CHECK_EQUAL(r[26], "00000000-0000-0000-0000-000000000000 %z#6 (piecewise (piece 0 (lt %x#6 1)) (piece (piecewise (piece 1 (lt %y#6 1)) (piece 2 (lt %y#6 2)) (otherwise 3)) (lt %x#6 2)) (otherwise (piecewise (piece 4 (lt %y#6 3)) (otherwise 5))))");
	BOOST_CHECK_EQUAL(r[27], "00000000-0000-0000-0000-000000000000 %z#0 (piecewise (piece 0 (lt %x#0 1)) (piece (piecewise (piece 1 (lt %y#0 1)) (piece 2 (lt %y#0 2)) (otherwise 3)) (lt %x#0 2)) (otherwise (piecewise (piece 4 (lt %y#0 3)) (otherwise 5))))");
}

BOOST_AUTO_TEST_CASE(function)
{
	SetupFunction();
	BOOST_CHECK(method::Rk4(input_db, "input", output_db));
	std::vector<std::string> rows{
		"00000000-0000-0000-0000-000000000000 %x#2 ($exponential_variate 10)",
		"00000000-0000-0000-0000-000000000000 %x#4 ($exponential_variate 10)",
		"00000000-0000-0000-0000-000000000000 %x#6 ($exponential_variate 10)",
		"00000000-0000-0000-0000-000000000000 %x#0 ($exponential_variate 10)",
		"00000000-0000-0000-0000-000000000000 %y#2 ($gamma_variate 2 1)",
		"00000000-0000-0000-0000-000000000000 %y#4 ($gamma_variate 2 1)",
		"00000000-0000-0000-0000-000000000000 %y#6 ($gamma_variate 2 1)",
		"00000000-0000-0000-0000-000000000000 %y#0 ($gamma_variate 2 1)",
		"00000000-0000-0000-0000-000000000000 %z#2 ($gauss_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %z#4 ($gauss_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %z#6 ($gauss_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %z#0 ($gauss_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %lnv#2 ($lognormal_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %lnv#4 ($lognormal_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %lnv#6 ($lognormal_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %lnv#0 ($lognormal_variate 0 1)",
		"00000000-0000-0000-0000-000000000000 %x0#2 ($poisson_variate 10)",
		"00000000-0000-0000-0000-000000000000 %x0#4 ($poisson_variate 10)",
		"00000000-0000-0000-0000-000000000000 %x0#6 ($poisson_variate 10)",
		"00000000-0000-0000-0000-000000000000 %x0#0 ($poisson_variate 10)",
		"00000000-0000-0000-0000-000000000000 %x1#2 ($uniform_variate 0 100 0 1)",
		"00000000-0000-0000-0000-000000000000 %x1#4 ($uniform_variate 0 100 0 1)",
		"00000000-0000-0000-0000-000000000000 %x1#6 ($uniform_variate 0 100 0 1)",
		"00000000-0000-0000-0000-000000000000 %x1#0 ($uniform_variate 0 100 0 1)",
		"00000000-0000-0000-0000-000000000000 %wv#2 ($weibull_variate 1 0.5)",
		"00000000-0000-0000-0000-000000000000 %wv#4 ($weibull_variate 1 0.5)",
		"00000000-0000-0000-0000-000000000000 %wv#6 ($weibull_variate 1 0.5)",
		"00000000-0000-0000-0000-000000000000 %wv#0 ($weibull_variate 1 0.5)",
		"00000000-0000-0000-0000-000000000000 %v#2 ($lookback %x#2 (minus (plus %time (divide @dt 2)) 0.001))",
		"00000000-0000-0000-0000-000000000000 %v#4 ($lookback %x#4 (minus (plus %time (divide @dt 2)) 0.001))",
		"00000000-0000-0000-0000-000000000000 %v#6 ($lookback %x#6 (minus (plus %time @dt) 0.001))",
		"00000000-0000-0000-0000-000000000000 %v#0 ($lookback %x#0 (minus (plus %time @dt) 0.001))",
		"00000000-0000-0000-0000-000000000000 %w#2 ($lookback %x#2 (minus (plus %time (divide @dt 2)) @dt))",
		"00000000-0000-0000-0000-000000000000 %w#4 ($lookback %x#4 (minus (plus %time (divide @dt 2)) @dt))",
		"00000000-0000-0000-0000-000000000000 %w#6 ($lookback %x#6 (minus (plus %time @dt) @dt))",
		"00000000-0000-0000-0000-000000000000 %w#0 ($lookback %x#0 (minus (plus %time @dt) @dt))",
		"00000000-0000-0000-0000-000000000000 %last_time#2 (minus (plus %time (divide @dt 2)) @dt)",
		"00000000-0000-0000-0000-000000000000 %last_time#4 (minus (plus %time (divide @dt 2)) @dt)",
		"00000000-0000-0000-0000-000000000000 %last_time#6 (minus (plus %time @dt) @dt)",
		"00000000-0000-0000-0000-000000000000 %last_time#0 (minus (plus %time @dt) @dt)",
		"00000000-0000-0000-0000-000000000000 %t2#2 (times (plus %time (divide @dt 2)) 2)",
		"00000000-0000-0000-0000-000000000000 %t2#4 (times (plus %time (divide @dt 2)) 2)",
		"00000000-0000-0000-0000-000000000000 %t2#6 (times (plus %time @dt) 2)",
		"00000000-0000-0000-0000-000000000000 %t2#0 (times (plus %time @dt) 2)",
	};
	output.CheckTable("asts", rows);
}

BOOST_AUTO_TEST_CASE(literal)
{
	SetupLiteral();
	BOOST_CHECK(method::Rk4(input_db, "input", output_db));
	std::vector<std::string> rows{
		"00000000-0000-0000-0000-000000000000 %e#2 2.71828182845904523536028747135266249775724709369995",
		"00000000-0000-0000-0000-000000000000 %e#4 2.71828182845904523536028747135266249775724709369995",
		"00000000-0000-0000-0000-000000000000 %e#6 2.71828182845904523536028747135266249775724709369995",
		"00000000-0000-0000-0000-000000000000 %e#0 2.71828182845904523536028747135266249775724709369995",
		"00000000-0000-0000-0000-000000000000 %x#2 1.2e-3",
		"00000000-0000-0000-0000-000000000000 %x#4 1.2e-3",
		"00000000-0000-0000-0000-000000000000 %x#6 1.2e-3",
		"00000000-0000-0000-0000-000000000000 %x#0 1.2e-3",
		"00000000-0000-0000-0000-000000000000 %y#2 45E-06",
		"00000000-0000-0000-0000-000000000000 %y#4 45E-06",
		"00000000-0000-0000-0000-000000000000 %y#6 45E-06",
		"00000000-0000-0000-0000-000000000000 %y#0 45E-06",
		"00000000-0000-0000-0000-000000000000 %z#2 (times -2.3e+30 .5E-10)",
		"00000000-0000-0000-0000-000000000000 %z#4 (times -2.3e+30 .5E-10)",
		"00000000-0000-0000-0000-000000000000 %z#6 (times -2.3e+30 .5E-10)",
		"00000000-0000-0000-0000-000000000000 %z#0 (times -2.3e+30 .5E-10)"
	};
	output.CheckTable("asts", rows);
}

BOOST_AUTO_TEST_CASE(ode)
{
	SetupOde();
	BOOST_CHECK(method::Rk4(input_db, "input", output_db));
	std::vector<std::string> r;
	output.Table("asts", &r);
	BOOST_CHECK_EQUAL(r.size(), 24u);
	BOOST_CHECK_EQUAL(r[0], "00000000-0000-0000-0000-000000000000 %x#1 (times @dt (minus %y))");
	BOOST_CHECK_EQUAL(r[1], "00000000-0000-0000-0000-000000000000 %x#2 (plus %x (divide %x#1 2))");
	BOOST_CHECK_EQUAL(r[2], "00000000-0000-0000-0000-000000000000 %x#3 (times @dt (minus %y#2))");
	BOOST_CHECK_EQUAL(r[3], "00000000-0000-0000-0000-000000000000 %x#4 (plus %x (divide %x#3 2))");
	BOOST_CHECK_EQUAL(r[4], "00000000-0000-0000-0000-000000000000 %x#5 (times @dt (minus %y#4))");
	BOOST_CHECK_EQUAL(r[5], "00000000-0000-0000-0000-000000000000 %x#6 (plus %x %x#5)");
	BOOST_CHECK_EQUAL(r[6], "00000000-0000-0000-0000-000000000000 %x#7 (times @dt (minus %y#6))");
	BOOST_CHECK_EQUAL(r[7], "00000000-0000-0000-0000-000000000000 %x#0 (plus %x (divide (plus %x#1 (plus (times 2 %x#3) (plus (times 2 %x#5) %x#7))) 6))");
	BOOST_CHECK_EQUAL(r[8], "00000000-0000-0000-0000-000000000000 %y#1 (times @dt (times %x 2))");
	BOOST_CHECK_EQUAL(r[9], "00000000-0000-0000-0000-000000000000 %y#2 (plus %y (divide %y#1 2))");
	BOOST_CHECK_EQUAL(r[10], "00000000-0000-0000-0000-000000000000 %y#3 (times @dt (times %x#2 2))");
	BOOST_CHECK_EQUAL(r[11], "00000000-0000-0000-0000-000000000000 %y#4 (plus %y (divide %y#3 2))");
	BOOST_CHECK_EQUAL(r[12], "00000000-0000-0000-0000-000000000000 %y#5 (times @dt (times %x#4 2))");
	BOOST_CHECK_EQUAL(r[13], "00000000-0000-0000-0000-000000000000 %y#6 (plus %y %y#5)");
	BOOST_CHECK_EQUAL(r[14], "00000000-0000-0000-0000-000000000000 %y#7 (times @dt (times %x#6 2))");
	BOOST_CHECK_EQUAL(r[15], "00000000-0000-0000-0000-000000000000 %y#0 (plus %y (divide (plus %y#1 (plus (times 2 %y#3) (plus (times 2 %y#5) %y#7))) 6))");
	BOOST_CHECK_EQUAL(r[16], "00000000-0000-0000-0000-000000000000 %z#1 (times @dt (plus %time %x))");
	BOOST_CHECK_EQUAL(r[17], "00000000-0000-0000-0000-000000000000 %z#2 (plus %z (divide %z#1 2))");
	BOOST_CHECK_EQUAL(r[18], "00000000-0000-0000-0000-000000000000 %z#3 (times @dt (plus (plus %time (divide @dt 2)) %x#2))");
	BOOST_CHECK_EQUAL(r[19], "00000000-0000-0000-0000-000000000000 %z#4 (plus %z (divide %z#3 2))");
	BOOST_CHECK_EQUAL(r[20], "00000000-0000-0000-0000-000000000000 %z#5 (times @dt (plus (plus %time (divide @dt 2)) %x#4))");
	BOOST_CHECK_EQUAL(r[21], "00000000-0000-0000-0000-000000000000 %z#6 (plus %z %z#5)");
	BOOST_CHECK_EQUAL(r[22], "00000000-0000-0000-0000-000000000000 %z#7 (times @dt (plus (plus %time @dt) %x#6))");
	BOOST_CHECK_EQUAL(r[23], "00000000-0000-0000-0000-000000000000 %z#0 (plus %z (divide (plus %z#1 (plus (times 2 %z#3) (plus (times 2 %z#5) %z#7))) 6))");
}

BOOST_AUTO_TEST_CASE(sbml)
{
	SetupSbml();
	BOOST_CHECK(method::Rk4(input_db, "input", output_db));
	std::vector<std::string> r;
	output.Table("asts", &r);
	BOOST_CHECK_EQUAL(r.size(), 8u);
	BOOST_CHECK_EQUAL(r[0], "00000000-0000-0000-0000-000000000000 %sbml:y#2 (power %sbml:x#2 2)");
	BOOST_CHECK_EQUAL(r[1], "00000000-0000-0000-0000-000000000000 %sbml:y#4 (power %sbml:x#4 2)");
	BOOST_CHECK_EQUAL(r[2], "00000000-0000-0000-0000-000000000000 %sbml:y#6 (power %sbml:x#6 2)");
	BOOST_CHECK_EQUAL(r[3], "00000000-0000-0000-0000-000000000000 %sbml:y#0 (power %sbml:x#0 2)");
	BOOST_CHECK_EQUAL(r[4], "00000000-0000-0000-0000-000000000000 %sbml:z#2 (times %sbml:y#2 0.5)");
	BOOST_CHECK_EQUAL(r[5], "00000000-0000-0000-0000-000000000000 %sbml:z#4 (times %sbml:y#4 0.5)");
	BOOST_CHECK_EQUAL(r[6], "00000000-0000-0000-0000-000000000000 %sbml:z#6 (times %sbml:y#6 0.5)");
	BOOST_CHECK_EQUAL(r[7], "00000000-0000-0000-0000-000000000000 %sbml:z#0 (times %sbml:y#0 0.5)");
}

BOOST_AUTO_TEST_SUITE_END()
