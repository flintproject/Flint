/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/tac.h"

#define BOOST_TEST_MODULE test_tac
#include "test.hh"

#include "db/driver.hh"

struct F {
	F()
		: driver("")
		, db(driver.db())
		, sql(db)
	{
		sql.Exec("CREATE TABLE sorts (uuid TEXT, name TEXT, math TEXT)");
	}

	void Setup(const char *math)
	{
		std::ostringstream oss;
		oss << "INSERT INTO sorts VALUES ('00000000-0000-0000-0000-000000000000', '%x', '"
			<< math
			<< "')";
		std::string query = oss.str();
		sql.Exec(query.c_str());
	}

	void Check(int nod, const char *body)
	{
		std::vector<std::string> r;
		sql.Table("tacs", &r);
		BOOST_CHECK_EQUAL(r.size(), 1u);
		std::ostringstream oss;
		oss << "00000000-0000-0000-0000-000000000000 %x "
			<< nod
			<< ' '
			<< body;
		std::string expected = oss.str();
		BOOST_CHECK_EQUAL(r[0], expected);
	}

	db::Driver driver;
	sqlite3 *db;
	test::Sql sql;
};

BOOST_FIXTURE_TEST_SUITE(test_tac, F)

BOOST_AUTO_TEST_CASE(Literal) {
	Setup("2.71828182845904523536028747135266249775724709369995");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(1,
		  "  loadi $0 2.71828182845904523536028747135266249775724709369995\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Delay) {
	Setup("($lookback %y (minus %time %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %time\n"
		  "  load $3 %z\n"
		  "  $1 = (minus $2 $3)\n"
		  "  lb $0 %y $1\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(DeltaTime) {
	Setup("($lookback %y (minus %time @dt))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %time\n"
		  "  load $3 @dt\n"
		  "  $1 = (minus $2 $3)\n"
		  "  lb $0 %y $1\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(ExponentialVariate) {
	Setup("($exponential_variate %y)");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(2,
		  "  load $1 %y\n"
		  "  $0 = ($exponential_variate $1)\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(GammaVariate) {
	Setup("($gamma_variate %y %z)");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(3,
		  "  load $1 %y\n"
		  "  load $2 %z\n"
		  "  $0 = ($gamma_variate $1 $2)\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(GaussVariate) {
	Setup("($gauss_variate %y %z)");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(3,
		  "  load $1 %y\n"
		  "  load $2 %z\n"
		  "  $0 = ($gauss_variate $1 $2)\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(UniformVariate) {
	Setup("($uniform_variate %y %z %a %b)");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(3,
		  "  load $1 %y\n"
		  "  load $2 %z\n"
		  "  $0 = ($uniform_variate $1 $2)\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(PoissonVariate) {
	Setup("($poisson_variate %y)");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(2,
		  "  load $1 %y\n"
		  "  $0 = ($poisson_variate $1)\n"
		  "  store %x $0\n"
		  );
}


BOOST_AUTO_TEST_CASE(Log1) {
	Setup("(log (plus %z 1))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %z\n"
		  "  loadi $3 1\n"
		  "  $1 = (plus $2 $3)\n"
		  "  $0 = (log10 $1)\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Log2) {
	Setup("(log (logbase 2) %y)");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(3,
		  "  loadi $1 2\n"
		  "  load $2 %y\n"
		  "  $0 = (log $1 $2)\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Neq) {
	Setup("(piecewise (piece %y (neq %a %b)) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (neq $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(And) {
	Setup("(piecewise (piece %y (and (leq %a %b) (leq %b %c))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(6,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (leq $2 $3)\n"
		  "  br $1 L3\n"
		  "  jmp L2\n"
		  " L3:\n"
		  "  load $4 %b\n"
		  "  load $5 %c\n"
		  "  $1 = (leq $4 $5)\n"
		  "  br $1 L1\n"
		  " L2:\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Or) {
	Setup("(piecewise (piece %y (or (leq %a %b) (leq %b %c))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(6,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (leq $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $4 %b\n"
		  "  load $5 %c\n"
		  "  $1 = (leq $4 $5)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Xor) {
	Setup("(piecewise (piece %y (xor (leq %a %b) (leq %b %c))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(8,
		  "  load $3 %a\n"
		  "  load $4 %b\n"
		  "  $2 = (leq $3 $4)\n"
		  "  load $6 %b\n"
		  "  load $7 %c\n"
		  "  $5 = (leq $6 $7)\n"
		  "  $1 = (neq $2 $5)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotAnd) {
	Setup("(piecewise (piece %y (not (and (leq %a %b) (leq %b %c)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(6,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (gt $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $4 %b\n"
		  "  load $5 %c\n"
		  "  $1 = (gt $4 $5)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotEq) {
	Setup("(piecewise (piece %y (not (eq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (neq $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotGeq) {
	Setup("(piecewise (piece %y (not (geq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (lt $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotGt) {
	Setup("(piecewise (piece %y (not (gt %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (leq $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotLeq) {
	Setup("(piecewise (piece %y (not (leq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (gt $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotLt) {
	Setup("(piecewise (piece %y (not (lt %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (geq $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotNeq) {
	Setup("(piecewise (piece %y (not (neq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (eq $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotNot) {
	Setup("(piecewise (piece %y (not (not (leq %a %b)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(4,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (leq $2 $3)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotOr) {
	Setup("(piecewise (piece %y (not (or (leq %a %b) (leq %b %c)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(6,
		  "  load $2 %a\n"
		  "  load $3 %b\n"
		  "  $1 = (gt $2 $3)\n"
		  "  br $1 L3\n"
		  "  jmp L2\n"
		  " L3:\n"
		  "  load $4 %b\n"
		  "  load $5 %c\n"
		  "  $1 = (gt $4 $5)\n"
		  "  br $1 L1\n"
		  " L2:\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotXor) {
	Setup("(piecewise (piece %y (not (xor (leq %a %b) (leq %b %c)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(8,
		  "  load $3 %a\n"
		  "  load $4 %b\n"
		  "  $2 = (leq $3 $4)\n"
		  "  load $6 %b\n"
		  "  load $7 %c\n"
		  "  $5 = (leq $6 $7)\n"
		  "  $1 = (eq $2 $5)\n"
		  "  br $1 L1\n"
		  "  load $0 %z\n"
		  "  jmp L0\n"
		  " L1:\n"
		  "  load $0 %y\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Trial) {
	Setup("($trial ($outcome 1 0.1) ($outcome 2 0.3) ($outcome (plus %y 1) 0.9))");
	BOOST_CHECK(compiler::tac::Tac(db));
	Check(12,
		  "  loadi $1 0\n"
		  "  loadi $2 1\n"
		  "  $3 = ($uniform_variate $1 $2)\n"
		  "  loadi $4 0.1\n"
		  "  $5 = (leq $3 $4)\n"
		  "  br $5 L1\n"
		  "  loadi $6 0.3\n"
		  "  $7 = (leq $3 $6)\n"
		  "  br $7 L2\n"
		  "  loadi $8 0.9\n"
		  "  $9 = (leq $3 $8)\n"
		  "  br $9 L3\n"
		  "  ret\n"
		  " L1:\n"
		  "  loadi $0 1\n"
		  "  jmp L0\n"
		  " L2:\n"
		  "  loadi $0 2\n"
		  "  jmp L0\n"
		  " L3:\n"
		  "  load $10 %y\n"
		  "  loadi $11 1\n"
		  "  $0 = (plus $10 $11)\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %x $0\n"
		  );
}

BOOST_AUTO_TEST_SUITE_END()
