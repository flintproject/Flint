/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method.hh"

#define BOOST_TEST_MODULE test_assign
#include "test.hh"

#include "f.hh"

BOOST_FIXTURE_TEST_SUITE(test_assign, F)

BOOST_AUTO_TEST_CASE(empty) {
	BOOST_CHECK(method::Assign(input_db, "input", output_db));
}

BOOST_AUTO_TEST_CASE(conditional) {
	SetupConditional();
	BOOST_CHECK(method::Assign(input_db, "input", output_db));
	std::vector<std::string> rows{
		"00000000000000000000000000000000 %a#0 (piecewise (piece (plus 1 pi) (eq %x#0 %y#0)))",
		"00000000000000000000000000000000 %b#0 (piecewise (piece 0 (leq %x#0 %y#0)) (otherwise 1))",
		"00000000000000000000000000000000 %c#0 (piecewise (piece 0 (eq %x#0 %y#0)) (piece 1 (lt %x#0 %y#0)))",
		"00000000000000000000000000000000 %d#0 (piecewise (piece 0 (eq %x#0 %y#0)) (piece 1 (gt %x#0 %y#0)) (otherwise 2))",
		"00000000000000000000000000000000 %x#0 (piecewise (piece (plus %y#0 1) (and (geq %time %a#0) (lt %time (plus %a#0 5)))) (otherwise 0))",
		"00000000000000000000000000000000 %y#0 (piecewise (piece %b#0 (eq %time 0.1)) (piece %c#0 (neq %time 0.2)))",
		"00000000000000000000000000000000 %z#0 (piecewise (piece 0 (lt %x#0 1)) (piece (piecewise (piece 1 (lt %y#0 1)) (piece 2 (lt %y#0 2)) (otherwise 3)) (lt %x#0 2)) (otherwise (piecewise (piece 4 (lt %y#0 3)) (otherwise 5))))"
	};
	CheckAsts(rows);
}

BOOST_AUTO_TEST_CASE(function)
{
	SetupFunction();
	BOOST_CHECK(method::Assign(input_db, "input", output_db));
	std::vector<std::string> rows{
		"00000000000000000000000000000000 %x#0 ($exponential_variate 10)",
		"00000000000000000000000000000000 %y#0 ($gamma_variate 2 1)",
		"00000000000000000000000000000000 %z#0 ($gauss_variate 0 1)",
		"00000000000000000000000000000000 %lnv#0 ($lognormal_variate 0 1)",
		"00000000000000000000000000000000 %x0#0 ($poisson_variate 10)",
		"00000000000000000000000000000000 %x1#0 ($uniform_variate 0 100 0 1)",
		"00000000000000000000000000000000 %wv#0 ($weibull_variate 1 0.5)",
		"00000000000000000000000000000000 %v#0 ($lookback %x#0 (minus %time 0.001))",
		"00000000000000000000000000000000 %w#0 ($lookback %x#0 (minus %time @dt))",
		"00000000000000000000000000000000 %last_time#0 (minus %time @dt)",
		"00000000000000000000000000000000 %t2#0 (times %time 2)"
	};
	CheckAsts(rows);
}

BOOST_AUTO_TEST_CASE(literal)
{
	SetupLiteral();
	BOOST_CHECK(method::Assign(input_db, "input", output_db));
	std::vector<std::string> rows{
		"00000000000000000000000000000000 %e#0 2.71828182845904523536028747135266249775724709369995",
		"00000000000000000000000000000000 %x#0 1.2e-3",
		"00000000000000000000000000000000 %y#0 45E-06",
		"00000000000000000000000000000000 %z#0 (times -2.3e+30 .5E-10)"
	};
	CheckAsts(rows);
}

BOOST_AUTO_TEST_CASE(sbml)
{
	SetupSbml();
	BOOST_CHECK(method::Assign(input_db, "input", output_db));
	std::vector<std::string> rows{
		"00000000000000000000000000000000 %sbml:y#0 (power %sbml:x#0 2)",
		"00000000000000000000000000000000 %sbml:z#0 (times %sbml:y#0 0.5)"
	};
	CheckAsts(rows);
}

BOOST_AUTO_TEST_SUITE_END()
