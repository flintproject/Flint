/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/tac.h"

#include "cas/dimension.h"
#include "db/helper.h"
#include "db/variable-inserter.h"

#define BOOST_TEST_MODULE test_tac
#include "test.hh"

struct F : public test::MemoryFixture {
	F()
		: db(driver_.db())
		, sql(db)
		, id(0)
		, vi()
		, da(new cas::DimensionAnalyzer)
	{
		sql.Exec("CREATE TABLE variables " VARIABLES_SCHEMA);
		vi.reset(new db::VariableInserter("variables", db));
		sql.Exec("CREATE TABLE sorts (uuid BLOB, name TEXT, math TEXT)");
	}

	void AddVariable(const char *name, int col = 1, int row = 1)
	{
		BOOST_CHECK(vi->Insert('v', ++id, name, col, row));
	}

	void Setup(const char *math)
	{
		AddVariable("a");
		AddVariable("b");
		AddVariable("c");
		AddVariable("x");
		AddVariable("y");
		AddVariable("z");
		AddVariable("v0", 1, 2);
		AddVariable("v1", 1, 3);
		AddVariable("v2", 3, 1);
		AddVariable("m0", 3, 3);
		AddVariable("m1", 2, 3);
		AddVariable("m2", 3, 2);
		BOOST_CHECK(da->Load(db));

		std::ostringstream oss;
		oss << "INSERT INTO sorts VALUES (X'00000000000000000000000000000000', '%Q', '"
			<< math
			<< "')";
		std::string query = oss.str();
		sql.Exec(query.c_str());
	}

	void Check(int noir, int nod, const char *body)
	{
		std::ostringstream oss;
		oss << "00000000000000000000000000000000 %Q "
			<< noir
			<< ' '
			<< nod
			<< ' '
			<< body;
		std::string expected = oss.str();
		std::vector<std::string> r{expected};
		sql.CheckRows("SELECT hex(uuid), name, noir, nod, body FROM tacs", r);
	}

	sqlite3 *db;
	test::Sql sql;
	int id;
	std::unique_ptr<db::VariableInserter> vi;
	std::unique_ptr<cas::DimensionAnalyzer> da;
};

BOOST_FIXTURE_TEST_SUITE(test_tac, F)

BOOST_AUTO_TEST_CASE(Literal) {
	Setup("2.71828182845904523536028747135266249775724709369995");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 1,
		  "  loadi $0 2.71828182845904523536028747135266249775724709369995\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Delay) {
	Setup("($lookback %y (minus %time %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
		  "  load $2 %time\n"
		  "  load $3 %z\n"
		  "  $1 = (minus $2 $3)\n"
		  "  lb $0 %y $1\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(DeltaTime) {
	Setup("($lookback %y (minus %time @dt))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
		  "  load $2 %time\n"
		  "  load $3 @dt\n"
		  "  $1 = (minus $2 $3)\n"
		  "  lb $0 %y $1\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(ExponentialVariate) {
	Setup("($exponential_variate %y)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 2,
		  "  load $1 %y\n"
		  "  $0 = ($exponential_variate $1)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(GammaVariate) {
	Setup("($gamma_variate %y %z)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 3,
		  "  load $1 %y\n"
		  "  load $2 %z\n"
		  "  $0 = ($gamma_variate $1 $2)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(GaussVariate) {
	Setup("($gauss_variate %y %z)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 3,
		  "  load $1 %y\n"
		  "  load $2 %z\n"
		  "  $0 = ($gauss_variate $1 $2)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(LognormalVariate) {
	Setup("($lognormal_variate %y %z)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 3,
		  "  load $1 %y\n"
		  "  load $2 %z\n"
		  "  $0 = ($lognormal_variate $1 $2)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(UniformVariate) {
	Setup("($uniform_variate %y %z %a %b)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 3,
		  "  load $1 %y\n"
		  "  load $2 %z\n"
		  "  $0 = ($uniform_variate $1 $2)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(WeibullVariate) {
	Setup("($weibull_variate %a %b)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 3,
		  "  load $1 %a\n"
		  "  load $2 %b\n"
		  "  $0 = ($weibull_variate $1 $2)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(PoissonVariate) {
	Setup("($poisson_variate %y)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 2,
		  "  load $1 %y\n"
		  "  $0 = ($poisson_variate $1)\n"
		  "  store %Q $0\n"
		  );
}


BOOST_AUTO_TEST_CASE(Log1) {
	Setup("(log (plus %z 1))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
		  "  load $2 %z\n"
		  "  loadi $3 1\n"
		  "  $1 = (plus $2 $3)\n"
		  "  $0 = (log10 $1)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Log2) {
	Setup("(log (logbase 2) %y)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 3,
		  "  loadi $1 2\n"
		  "  load $2 %y\n"
		  "  $0 = (log $1 $2)\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Neq) {
	Setup("(piecewise (piece %y (neq %a %b)) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(And) {
	Setup("(piecewise (piece %y (and (leq %a %b) (leq %b %c))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 6,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Or) {
	Setup("(piecewise (piece %y (or (leq %a %b) (leq %b %c))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 6,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Xor) {
	Setup("(piecewise (piece %y (xor (leq %a %b) (leq %b %c))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 8,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotAnd) {
	Setup("(piecewise (piece %y (not (and (leq %a %b) (leq %b %c)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 6,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotEq) {
	Setup("(piecewise (piece %y (not (eq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotGeq) {
	Setup("(piecewise (piece %y (not (geq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotGt) {
	Setup("(piecewise (piece %y (not (gt %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotLeq) {
	Setup("(piecewise (piece %y (not (leq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotLt) {
	Setup("(piecewise (piece %y (not (lt %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotNeq) {
	Setup("(piecewise (piece %y (not (neq %a %b))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotNot) {
	Setup("(piecewise (piece %y (not (not (leq %a %b)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 4,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotOr) {
	Setup("(piecewise (piece %y (not (or (leq %a %b) (leq %b %c)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 6,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(NotXor) {
	Setup("(piecewise (piece %y (not (xor (leq %a %b) (leq %b %c)))) (otherwise %z))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 8,
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
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(Trial) {
	Setup("($trial ($outcome 1 0.1) ($outcome 2 0.3) ($outcome (plus %y 1) 0.9))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(0, 15,
		  "  loadi $1 0\n"
		  "  loadi $2 1\n"
		  "  $3 = ($uniform_variate $1 $2)\n"
		  "  loadi $4 0.1\n"
		  "  $5 = (minus $3 $4)\n"
		  "  $6 = (leq $5 $1)\n"
		  "  br $6 L1\n"
		  "  loadi $7 0.3\n"
		  "  $8 = (minus $5 $7)\n"
		  "  $9 = (leq $8 $1)\n"
		  "  br $9 L2\n"
		  "  loadi $10 0.9\n"
		  "  $11 = (minus $8 $10)\n"
		  "  $12 = (leq $11 $1)\n"
		  "  br $12 L3\n"
		  "  ret\n"
		  " L1:\n"
		  "  loadi $0 1\n"
		  "  jmp L0\n"
		  " L2:\n"
		  "  loadi $0 2\n"
		  "  jmp L0\n"
		  " L3:\n"
		  "  load $13 %y\n"
		  "  loadi $14 1\n"
		  "  $0 = (plus $13 $14)\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %Q $0\n"
		  );
}

BOOST_AUTO_TEST_CASE(SelectorA) {
	Setup("(selector %v0 1)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(1, 2,
		  "  refer $i0 %v0\n"
		  "  loadi $1 1\n"
		  "  select2 $0 $i0 $1\n"
		  "  store %Q $0\n");
}

BOOST_AUTO_TEST_CASE(SelectorB) {
	Setup("(selector %m0 3 2)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(1, 3,
		  "  refer $i0 %m0\n"
		  "  loadi $1 3\n"
		  "  loadi $2 2\n"
		  "  select3 $0 3 3 $i0 $1 $2\n"
		  "  store %Q $0\n");
}

BOOST_AUTO_TEST_CASE(SelectorC) {
	Setup("(selector %m1 3)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(2, 1,
		  "  refer $i1 %m1\n"
		  "  loadi $0 3\n"
		  "  selrow $i0 3 2 $i1 $0\n"
		  "  save %Q $i0 2\n");
}

BOOST_AUTO_TEST_CASE(Vector) {
	Setup("(vector %x %y %z 1.25)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(1, 4,
		  "  alloc $i0 4\n"
		  "  load $0 %x\n"
		  "  move $i0 $0 0\n"
		  "  load $1 %y\n"
		  "  move $i0 $1 1\n"
		  "  load $2 %z\n"
		  "  move $i0 $2 2\n"
		  "  loadi $3 1.25\n"
		  "  move $i0 $3 3\n"
		  "  save %Q $i0 4\n");
}

BOOST_AUTO_TEST_CASE(Matrix) {
	Setup("(matrix (matrixrow 1 0) (matrixrow %x %y) (matrixrow 0 -1))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(1, 6,
		  "  alloc $i0 6\n"
		  "  loadi $0 1\n"
		  "  move $i0 $0 0\n"
		  "  loadi $1 0\n"
		  "  move $i0 $1 1\n"
		  "  load $2 %x\n"
		  "  move $i0 $2 2\n"
		  "  load $3 %y\n"
		  "  move $i0 $3 3\n"
		  "  loadi $4 0\n"
		  "  move $i0 $4 4\n"
		  "  loadi $5 -1\n"
		  "  move $i0 $5 5\n"
		  "  save %Q $i0 6\n");
}

BOOST_AUTO_TEST_CASE(TimesWithScalar) {
	Setup("(times %a %m0)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(2, 1,
		  "  load $0 %a\n"
		  "  refer $i1 %m0\n"
		  "  mult $i0 9 $0 $i1\n"
		  "  save %Q $i0 9\n");
}

BOOST_AUTO_TEST_CASE(TimesForMatrices) {
	Setup("(times %m1 %m2)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(3, 0,
		  "  refer $i1 %m1\n"
		  "  refer $i2 %m2\n"
		  "  mmul $i0 3 2 3 $i1 $i2\n"
		  "  save %Q $i0 9\n");
}

BOOST_AUTO_TEST_CASE(Transpose) {
	Setup("(transpose %m1)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(2, 0,
		  "  refer $i1 %m1\n"
		  "  transpose $i0 $i1 2 3\n"
		  "  save %Q $i0 6\n");
}

BOOST_AUTO_TEST_CASE(Outerproduct) {
	Setup("(outerproduct %v0 (transpose %v1))");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(4, 0,
		  "  refer $i1 %v0\n"
		  "  refer $i3 %v1\n"
		  "  transpose $i2 $i3 1 3\n"
		  "  outerproduct $i0 2 $i1 3 $i2\n"
		  "  save %Q $i0 6\n");
}

BOOST_AUTO_TEST_CASE(Scalarproduct) {
	Setup("(scalarproduct %v2 %v2)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(2, 1,
		  "  refer $i0 %v2\n"
		  "  refer $i1 %v2\n"
		  "  scalarproduct $0 3 $i0 $i1\n"
		  "  store %Q $0\n");
}

BOOST_AUTO_TEST_CASE(Vectorproduct) {
	Setup("(vectorproduct %v1 %v1)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(3, 0,
		  "  refer $i1 %v1\n"
		  "  refer $i2 %v1\n"
		  "  vectorproduct $i0 $i1 $i2\n"
		  "  save %Q $i0 3\n");
}

BOOST_AUTO_TEST_CASE(Determinant) {
	Setup("(determinant %m0)");
	BOOST_CHECK(compiler::tac::Tac(da.get(), db));
	Check(1, 1,
		  "  refer $i0 %m0\n"
		  "  determinant $0 3 $i0\n"
		  "  store %Q $0\n");
}

BOOST_AUTO_TEST_SUITE_END()
