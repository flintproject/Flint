/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method.hh"

#define BOOST_TEST_MODULE test_assign
#include "test.hh"

#include "f.hh"

BOOST_FIXTURE_TEST_SUITE(test_assign, F)

BOOST_AUTO_TEST_CASE(empty) {
	BOOST_CHECK(method::Assign(db, "input", "output"));
}

BOOST_AUTO_TEST_CASE(conditional) {
	SetupConditional();
	BOOST_CHECK(method::Assign(db, "input", "output"));
	std::vector<std::string> r;
	sql.Table("output", &r);
	BOOST_CHECK_EQUAL(r.size(), 7u);
	BOOST_CHECK_EQUAL(r[0], "00000000-0000-0000-0000-000000000000 %a#0 (piecewise (piece (plus 1 pi) (eq %x#0 %y#0)))");
	BOOST_CHECK_EQUAL(r[1], "00000000-0000-0000-0000-000000000000 %b#0 (piecewise (piece 0 (leq %x#0 %y#0)) (otherwise 1))");
	BOOST_CHECK_EQUAL(r[2], "00000000-0000-0000-0000-000000000000 %c#0 (piecewise (piece 0 (eq %x#0 %y#0)) (piece 1 (lt %x#0 %y#0)))");
	BOOST_CHECK_EQUAL(r[3], "00000000-0000-0000-0000-000000000000 %d#0 (piecewise (piece 0 (eq %x#0 %y#0)) (piece 1 (gt %x#0 %y#0)) (otherwise 2))");
	BOOST_CHECK_EQUAL(r[4], "00000000-0000-0000-0000-000000000000 %x#0 (piecewise (piece (plus %y#0 1) (and (geq %time %a#0) (lt %time (plus %a#0 5)))) (otherwise 0))");
	BOOST_CHECK_EQUAL(r[5], "00000000-0000-0000-0000-000000000000 %y#0 (piecewise (piece %b#0 (eq %time 0.1)) (piece %c#0 (neq %time 0.2)))");
	BOOST_CHECK_EQUAL(r[6], "00000000-0000-0000-0000-000000000000 %z#0 (piecewise (piece 0 (lt %x#0 1)) (piece (piecewise (piece 1 (lt %y#0 1)) (piece 2 (lt %y#0 2)) (otherwise 3)) (lt %x#0 2)) (otherwise (piecewise (piece 4 (lt %y#0 3)) (otherwise 5))))");
}

BOOST_AUTO_TEST_CASE(function)
{
	SetupFunction();
	BOOST_CHECK(method::Assign(db, "input", "output"));
	std::vector<std::string> r;
	sql.Table("output", &r);
	BOOST_CHECK_EQUAL(r.size(), 8u);
	BOOST_CHECK_EQUAL(r[0], "00000000-0000-0000-0000-000000000000 %x#0 ($exponential_variate 10)");
	BOOST_CHECK_EQUAL(r[1], "00000000-0000-0000-0000-000000000000 %y#0 ($gamma_variate 2 1)");
	BOOST_CHECK_EQUAL(r[2], "00000000-0000-0000-0000-000000000000 %z#0 ($gauss_variate 0 1)");
	BOOST_CHECK_EQUAL(r[3], "00000000-0000-0000-0000-000000000000 %x0#0 ($poisson_variate 10)");
	BOOST_CHECK_EQUAL(r[4], "00000000-0000-0000-0000-000000000000 %x1#0 ($uniform_variate 0 100 0 1)");
	BOOST_CHECK_EQUAL(r[5], "00000000-0000-0000-0000-000000000000 %v#0 ($lookback %x#0 (minus %time 0.001))");
	BOOST_CHECK_EQUAL(r[6], "00000000-0000-0000-0000-000000000000 %w#0 ($lookback %x#0 (minus %time @dt))");
	BOOST_CHECK_EQUAL(r[7], "00000000-0000-0000-0000-000000000000 %t2#0 (times %time 2)");
}

BOOST_AUTO_TEST_CASE(literal)
{
	SetupLiteral();
	BOOST_CHECK(method::Assign(db, "input", "output"));
	std::vector<std::string> r;
	sql.Table("output", &r);
	BOOST_CHECK_EQUAL(r.size(), 3u);
	BOOST_CHECK_EQUAL(r[0], "00000000-0000-0000-0000-000000000000 %x#0 0.0012");
	BOOST_CHECK_EQUAL(r[1], "00000000-0000-0000-0000-000000000000 %y#0 4.5e-05");
	BOOST_CHECK_EQUAL(r[2], "00000000-0000-0000-0000-000000000000 %z#0 (times -2.3e+30 5e-11)");
}

BOOST_AUTO_TEST_CASE(sbml)
{
	SetupSbml();
	BOOST_CHECK(method::Assign(db, "input", "output"));
	std::vector<std::string> r;
	sql.Table("output", &r);
	BOOST_CHECK_EQUAL(r.size(), 2u);
	BOOST_CHECK_EQUAL(r[0], "00000000-0000-0000-0000-000000000000 %sbml:y#0 (power %sbml:x#0 2)");
	BOOST_CHECK_EQUAL(r[1], "00000000-0000-0000-0000-000000000000 %sbml:z#0 (times %sbml:y#0 0.5)");
}

BOOST_AUTO_TEST_SUITE_END()
