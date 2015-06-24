/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/bcc.h"

#define BOOST_TEST_MODULE test_bcc
#include "test.hh"

#include "db/driver.hh"
#include "db/query.h"
#include "db/tac-inserter.hh"

struct F {
	F()
		: driver("")
		, db(driver.db())
		, sql(db)
	{
		BOOST_CHECK_EQUAL(SaveNol(1, db), 1);
		sql.Exec("CREATE TABLE tacs (uuid TEXT, name TEXT, nod INTEGER, body TEXT)");
	}

	void Setup(const char *uuid, const char *name, int nod, const char *body)
	{
		db::TacInserter ti(db);
		BOOST_CHECK(ti.Insert(uuid, name, nod, body));
	}

	void SetupCall1(const char *f)
	{
		std::ostringstream oss;
		oss << "  load $1 %a\n"
			<< "  $0 = (" << f << " $1)\n"
			<< "  store %x $0\n";
		std::string body = oss.str();
		Setup("00000000-0000-0000-0000-000000000000", "%x", 2, body.c_str());
	}

	void SetupCall2(const char *f)
	{
		std::ostringstream oss;
		oss << "  load $1 %a\n"
			<< "  load $2 %b\n"
			<< "  $0 = (" << f << " $1 $2)\n"
			<< "  store %x $0\n";
		std::string body = oss.str();
		Setup("00000000-0000-0000-0000-000000000000", "%x", 3, body.c_str());
	}

	db::Driver driver;
	sqlite3 *db;
	test::Sql sql;
};

BOOST_FIXTURE_TEST_SUITE(test_bcc, F)

BOOST_AUTO_TEST_CASE(Abs) {
	SetupCall1("abs");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arccos) {
	SetupCall1("arccos");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arccosh) {
	SetupCall1("arccosh");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arccot) {
	SetupCall1("arccot");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arccoth) {
	SetupCall1("arccoth");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arccsc) {
	SetupCall1("arccsc");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arccsch) {
	SetupCall1("arccsch");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arcsec) {
	SetupCall1("arcsec");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arcsech) {
	SetupCall1("arcsech");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arcsin) {
	SetupCall1("arcsin");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arcsinh) {
	SetupCall1("arcsinh");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arctan) {
	SetupCall1("arctan");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Arctanh) {
	SetupCall1("arctanh");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Ceiling) {
	SetupCall1("ceiling");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Cos) {
	SetupCall1("cos");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Cosh) {
	SetupCall1("cosh");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Cot) {
	SetupCall1("cot");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Coth) {
	SetupCall1("coth");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Csc) {
	SetupCall1("csc");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Csch) {
	SetupCall1("csch");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Exp) {
	SetupCall1("exp");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Floor) {
	SetupCall1("floor");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Ln) {
	SetupCall1("ln");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Log10) {
	SetupCall1("log10");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Sec) {
	SetupCall1("sec");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Sech) {
	SetupCall1("sech");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Sin) {
	SetupCall1("sin");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Sinh) {
	SetupCall1("sinh");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Tan) {
	SetupCall1("tan");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Tanh) {
	SetupCall1("tanh");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(ExponentialVariate) {
	SetupCall1("$exponential_variate");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(PoissonVariate) {
	SetupCall1("$poisson_variate");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Eq) {
	SetupCall2("eq");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Geq) {
	SetupCall2("geq");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Gt) {
	SetupCall2("gt");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Leq) {
	SetupCall2("leq");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Log) {
	SetupCall2("log");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Lt) {
	SetupCall2("lt");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Max) {
	SetupCall2("max");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Min) {
	SetupCall2("min");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Neq) {
	SetupCall2("neq");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Plus) {
	SetupCall2("plus");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Power) {
	SetupCall2("power");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(Times) {
	SetupCall2("times");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(GammaVariate) {
	SetupCall2("$gamma_variate");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(GaussVariate) {
	SetupCall2("$gauss_variate");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(UniformVariate) {
	SetupCall2("$uniform_variate");
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_CASE(DuplicateLabels) {
	Setup("72b52587-043b-4950-98bb-b29c97237140",
		  "%boolin",
		  52,
		  "  load $2 %dvdt\n"
		  "  loadi $3 150\n"
		  "  $1 = (gt $2 $3)\n"
		  "  br $1 L3\n"
		  "  jmp L2\n"
		  " L3:\n"
		  "  load $4 %boolin\n"
		  "  loadi $5 0\n"
		  "  $1 = (leq $4 $5)\n"
		  "  br $1 L1\n"
		  " L2:\n"
		  "  load $7 %timer\n"
		  "  loadi $8 2\n"
		  "  $6 = (gt $7 $8)\n"
		  "  br $6 L8\n"
		  "  jmp L7\n"
		  " L8:\n"
		  "  load $9 %boolin\n"
		  "  loadi $10 0\n"
		  "  $6 = (gt $9 $10)\n"
		  "  br $6 L6\n"
		  " L7:\n"
		  "  jmp L5\n"
		  " L6:\n"
		  "  load $11 %boolin\n"
		  "  loadi $12 1\n"
		  "  $6 = (leq $11 $12)\n"
		  "  br $6 L4\n"
		  " L5:\n"
		  "  load $14 %boolin\n"
		  "  loadi $15 1\n"
		  "  $13 = (gt $14 $15)\n"
		  "  br $13 L13\n"
		  "  jmp L12\n"
		  " L13:\n"
		  "  load $16 %boolin\n"
		  "  loadi $17 2\n"
		  "  $13 = (leq $16 $17)\n"
		  "  br $13 L11\n"
		  " L12:\n"
		  "  jmp L10\n"
		  " L11:\n"
		  "  load $18 %time\n"
		  "  load $19 %time_start\n"
		  "  $13 = (geq $18 $19)\n"
		  "  br $13 L17\n"
		  "  jmp L16\n"
		  " L17:\n"
		  "  load $20 %time\n"
		  "  load $22 %time_start\n"
		  "  load $27 %time\n"
		  "  load $28 %time_start\n"
		  "  $26 = (minus $27 $28)\n"
		  "  load $30 %width\n"
		  "  load $31 %interval\n"
		  "  $29 = (plus $30 $31)\n"
		  "  $25 = (divide $26 $29)\n"
		  "  $24 = (floor $25)\n"
		  "  load $33 %width\n"
		  "  load $34 %interval\n"
		  "  $32 = (plus $33 $34)\n"
		  "  $23 = (times $24 $32)\n"
		  "  $21 = (plus $22 $23)\n"
		  "  $13 = (geq $20 $21)\n"
		  "  br $13 L15\n"
		  " L16:\n"
		  "  jmp L14\n"
		  " L15:\n"
		  "  load $35 %time\n"
		  "  load $38 %time_start\n"
		  "  load $39 %width\n"
		  "  $37 = (plus $38 $39)\n"
		  "  load $44 %time\n"
		  "  load $45 %time_start\n"
		  "  $43 = (minus $44 $45)\n"
		  "  load $47 %width\n"
		  "  load $48 %interval\n"
		  "  $46 = (plus $47 $48)\n"
		  "  $42 = (divide $43 $46)\n"
		  "  $41 = (floor $42)\n"
		  "  load $50 %width\n"
		  "  load $51 %interval\n"
		  "  $49 = (plus $50 $51)\n"
		  "  $40 = (times $41 $49)\n"
		  "  $36 = (plus $37 $40)\n"
		  "  $13 = (leq $35 $36)\n"
		  "  br $13 L9\n"
		  " L14:\n"
		  " L10:\n"
		  "  ret\n"
		  " L1:\n"
		  "  loadi $0 1\n"
		  "  jmp L0\n"
		  " L4:\n"
		  "  loadi $0 2\n"
		  "  jmp L0\n"
		  " L9:\n"
		  "  loadi $0 0\n"
		  "  jmp L0\n"
		  " L0:\n"
		  "  store %boolin $0\n"
		  );
	BOOST_CHECK(compiler::bcc::Bcc(db, &std::cout));
}

BOOST_AUTO_TEST_SUITE_END()
