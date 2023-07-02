/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler/bcc.h"
#include "compiler/bcc/token.h"
#include "compiler/bcc/tokenizer.h"

#include <boost/uuid/string_generator.hpp>

#define BOOST_TEST_MODULE test_bcc
#include "test.h"

#include "db/helper.h"
#include "db/query.h"
#include "db/tac-inserter.h"
#include "flint/bc.h"

struct F : test::MemoryFixture {
	F()
		: db(driver_.db())
		, sql(db)
	{
		BOOST_CHECK_EQUAL(SaveNol(1, db), 1);
		sql.Exec("CREATE TABLE tacs " TACS_SCHEMA);
	}

	void Setup(const char *uuid, const char *name, int noir, int nod, const char *body)
	{
		db::TacInserter ti(db);
		boost::uuids::string_generator gen;
		boost::uuids::uuid u = gen(uuid);
		BOOST_CHECK(ti.Insert(u, name, noir, nod, body));
	}

	void Setup(const char *name, int noir, int nod, const char *body)
	{
		db::TacInserter ti(db);
		BOOST_CHECK(ti.Insert(name, noir, nod, body));
	}

	void Setup(const char *name, int nod, const char *body)
	{
		db::TacInserter ti(db);
		BOOST_CHECK(ti.Insert(name, 0, nod, body));
	}

	void SetupCall1(const char *f)
	{
		std::ostringstream oss;
		oss << "  load $1 %a\n"
			<< "  $0 = (" << f << " $1)\n"
			<< "  store %x $0\n";
		std::string body = oss.str();
		Setup("%x", 2, body.c_str());
	}

	void SetupCall2(const char *f)
	{
		std::ostringstream oss;
		oss << "  load $1 %a\n"
			<< "  load $2 %b\n"
			<< "  $0 = (" << f << " $1 $2)\n"
			<< "  store %x $0\n";
		std::string body = oss.str();
		Setup("%x", 3, body.c_str());
	}

	void SetupConstant(const char *f)
	{
		std::ostringstream oss;
		oss << "  loadi $0 " << f << "\n"
			<< "  store %c $0\n";
		std::string body = oss.str();
		Setup("%c", 1, body.c_str());
	}

	void DoCheck()
	{
		std::unique_ptr<Bytecode> bc(compiler::bcc::Bcc(db));
		BOOST_CHECK(bc);
	}

	sqlite3 *db;
	test::Sql sql;
};

BOOST_FIXTURE_TEST_SUITE(test_bcc, F)

BOOST_AUTO_TEST_CASE(Abs) {
	SetupCall1("abs");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arccos) {
	SetupCall1("arccos");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arccosh) {
	SetupCall1("arccosh");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arccot) {
	SetupCall1("arccot");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arccoth) {
	SetupCall1("arccoth");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arccsc) {
	SetupCall1("arccsc");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arccsch) {
	SetupCall1("arccsch");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arcsec) {
	SetupCall1("arcsec");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arcsech) {
	SetupCall1("arcsech");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arcsin) {
	SetupCall1("arcsin");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arcsinh) {
	SetupCall1("arcsinh");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arctan) {
	SetupCall1("arctan");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Arctanh) {
	SetupCall1("arctanh");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Ceiling) {
	SetupCall1("ceiling");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Cos) {
	SetupCall1("cos");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Cosh) {
	SetupCall1("cosh");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Cot) {
	SetupCall1("cot");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Coth) {
	SetupCall1("coth");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Csc) {
	SetupCall1("csc");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Csch) {
	SetupCall1("csch");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Exp) {
	SetupCall1("exp");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Factorial) {
	SetupCall1("factorial");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Floor) {
	SetupCall1("floor");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Ln) {
	SetupCall1("ln");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Log10) {
	SetupCall1("log10");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Sec) {
	SetupCall1("sec");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Sech) {
	SetupCall1("sech");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Sin) {
	SetupCall1("sin");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Sinh) {
	SetupCall1("sinh");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Tan) {
	SetupCall1("tan");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Tanh) {
	SetupCall1("tanh");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(ExponentialVariate) {
	SetupCall1("$exponential_variate");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(PoissonVariate) {
	SetupCall1("$poisson_variate");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Eq) {
	SetupCall2("eq");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Geq) {
	SetupCall2("geq");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Gt) {
	SetupCall2("gt");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Leq) {
	SetupCall2("leq");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Log) {
	SetupCall2("log");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Lt) {
	SetupCall2("lt");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Max) {
	SetupCall2("max");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Min) {
	SetupCall2("min");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Minus1) {
	SetupCall1("minus");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Minus2) {
	SetupCall2("minus");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Neq) {
	SetupCall2("neq");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Plus) {
	SetupCall2("plus");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Root1) {
	SetupCall1("root");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Root2) {
	SetupCall2("root");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Power) {
	SetupCall2("power");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Times) {
	SetupCall2("times");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(GammaVariate) {
	SetupCall2("$gamma_variate");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(GaussVariate) {
	SetupCall2("$gauss_variate");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(LognormalVariate) {
	SetupCall2("$lognormal_variate");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(UniformVariate) {
	SetupCall2("$uniform_variate");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(WeibullVariate) {
	SetupCall2("$weibull_variate");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Eulergamma) {
	SetupConstant("eulergamma");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Exponentiale) {
	SetupConstant("exponentiale");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Pi) {
	SetupConstant("pi");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(True) {
	SetupConstant("true");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(False) {
	SetupConstant("false");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(DuplicateLabels) {
	Setup("72b52587-043b-4950-98bb-b29c97237140",
		  "%boolin",
		  0,
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
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Refer) {
	Setup("%c", 1, 0, "  refer $i0 %foo\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Deref) {
	Setup("%c", 101, 1, "  deref $0 $i100 777\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Alloc) {
	Setup("%c", 6, 0, "  alloc $i5 24\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Save) {
	Setup("%c", 1, 0, "  save %you $i0 10\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Move) {
	Setup("%c", 8, 5, "  move $i7 $4 2\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Transpose) {
	Setup("%c", 2, 0, "  transpose $i0 $i1 2 3\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Outerproduct) {
	Setup("%c", 21, 0, "  outerproduct $i0 10 $i10 20 $i20\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Scalarproduct) {
	Setup("%c", 9, 1, "  scalarproduct $0 6 $i7 $i8\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Vectorproduct) {
	Setup("%c", 3, 0, "  vectorproduct $i0 $i1 $i2\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Determinant) {
	Setup("%c", 1, 1, "  determinant $0 3 $i0\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Select2) {
	Setup("%c", 1, 2, "  select2 $0 $i0 $1\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Select3) {
	Setup("%c", 2, 4, "  select3 $0 2 3 $i1 $2 $3\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Selrow) {
	Setup("%c", 2, 1, "  selrow $i0 2 3 $i1 $0\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Mult) {
	Setup("%c", 3, 1, "  mult $i0 5 $0 $i2\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(Mmul) {
	Setup("%c", 3, 0, "  mmul $i0 1 2 3 $i1 $i2\n");
	DoCheck();
}

BOOST_AUTO_TEST_CASE(tokenizer) {
	compiler::bcc::Tokenizer lex("  $0 = (arccot $1 $2)\n");
	compiler::bcc::Token t;
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kSpace);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kSpace);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kAddress);
	BOOST_CHECK_EQUAL(t.size, 2);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kSpace);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kEqualSign);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kSpace);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kParenOpen);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kArccot);
	BOOST_CHECK_EQUAL(t.size, 6);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kSpace);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kAddress);
	BOOST_CHECK_EQUAL(t.size, 2);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kSpace);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kAddress);
	BOOST_CHECK_EQUAL(t.size, 2);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kParenClose);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 1);
	BOOST_CHECK(t.type == compiler::bcc::Token::Type::kEol);
	BOOST_CHECK_EQUAL(t.size, 1);
	BOOST_CHECK_EQUAL(lex(&t), 0);
}

BOOST_AUTO_TEST_SUITE_END()
