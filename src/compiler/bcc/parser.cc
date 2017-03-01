/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compiler/bcc/parser.h"

#include <cassert>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/math/constants/constants.hpp>

#include "compiler/bcc/token.h"
#include "compiler/bcc/tokenizer.h"
#include "flint/bc.h"

namespace flint {
namespace compiler {
namespace bcc {

namespace {

void ReportUnexpectedToken(Token &t)
{
	std::cerr << "unexpected token: ";
	t.Write(std::cerr) << std::endl;
}

int ConvertToInteger(const char *p, const char *q)
{
	assert(p < q);
	int n = 0;
	while (p < q) {
		n *= 10;
		char c = *p++;
		assert('0' <= c && c <= '9');
		n += c - '0';
	}
	return n;
}

int GetIntegerFromAddress(const Token &t)
{
	assert(t.type == Token::Type::kAddress);
	const char *p = t.lexeme + 1; // add offset for "$"
	const char *q = t.lexeme + t.size;
	return ConvertToInteger(p, q);
}

int GetIntegerFromLabel(const Token &t)
{
	assert(t.type == Token::Type::kLabel);
	const char *p = t.lexeme + 1; // add offset for "L"
	const char *q = t.lexeme + t.size;
	return ConvertToInteger(p, q);
}

int GetIntegerFromNumber(const Token &t)
{
	assert(t.type == Token::Type::kNumber);
	const char *p = t.lexeme;
	const char *q = t.lexeme + t.size;
	return ConvertToInteger(p, q);
}

int GetIntegerFromIntReg(const Token &t)
{
	assert(t.type == Token::Type::kIntReg);
	const char *p = t.lexeme + 2; // offset for "$i"
	const char *q = t.lexeme + t.size; // subtract offset too
	return ConvertToInteger(p, q);
}

}

class ParserImpl {
public:
	explicit ParserImpl(const char *input);

	bool Parse(Body *body);

private:
	bool ParseInst(bc::Code *code);
	bool ParseLabelColon(int n, Body *body);
	bool ParseInstOp(int a, bc::Code *code);
	bool ParseCall1(int a, bc::Call1::Op op, bc::Code *code);
	bool ParseCall2(int a, bc::Call2::Op op, bc::Code *code);
	bool ParseGen1(int a, bc::Gen1::Type type, bc::Code *code);
	bool ParseGen2(int a, bc::Gen2::Type type, bc::Code *code);
	bool ParseFr(int *a);
	bool ParseInstBr(bc::Code *code);
	bool ParseLabel(int *n);
	bool ParseInstJmp(bc::Code *code);
	bool ParseInstLb(bc::Code *code);
	bool ParseInstLc(bc::Code *code);
	bool ParseInstLd(bc::Code *code);
	bool ParseInstLoad(bc::Code *code);
	bool ParseInstLoadi(bc::Code *code);
	bool ParseImm(double *d);
	bool ParseInstStore(bc::Code *code);
	bool ParseInstRefer(bc::Code *code);
	bool ParseIr(int *i);
	bool ParseInstDeref(bc::Code *code);
	bool ParseInstAlloc(bc::Code *code);
	bool ParseInstSave(bc::Code *code);
	bool ParseInstMove(bc::Code *code);
	bool ParseInstTranspose(bc::Code *code);
	bool ParseInstOuterproduct(bc::Code *code);
	bool ParseInstScalarproduct(bc::Code *code);
	bool ParseInstVectorproduct(bc::Code *code);
	bool ParseInstDeterminant(bc::Code *code);
	bool ParseInstSelect2(bc::Code *code);
	bool ParseInstSelect3(bc::Code *code);
	bool ParseInstSelrow(bc::Code *code);
	bool ParseInstMult(bc::Code *code);
	bool ParseInstMmul(bc::Code *code);

	bool ReadIdentifier(Token *t);
	bool ReadInteger(int *i);
	bool ReadToken(Token::Type type);
	bool ReadToken(Token::Type type, Token *t);
	bool ReadToken(Token *t);
	bool SkipParenClose();
	bool SkipSpace();

	template<bc::Call1::Op op1, bc::Call2::Op op2>
	bool ParseCall(int a, bc::Code *code) {
		int a1;
		if (!ParseFr(&a1))
			return false;
		Token t;
		if (!ReadToken(&t))
			return false;
		switch (t.type) {
		case Token::Type::kParenClose:
			{
				code->set_type(bc::Code::kCall1);
				auto *call1 = code->mutable_call1();
				call1->set_a(a);
				call1->set_op(op1);
				call1->set_a1(a1);
				return true;
			}
		case Token::Type::kSpace:
			// one more argument
			break;
		default:
			ReportUnexpectedToken(t);
			return false;
		}
		if ( !ReadToken(Token::Type::kAddress, &t) ||
			 !SkipParenClose() )
			return false;
		code->set_type(bc::Code::kCall2);
		auto *call2 = code->mutable_call2();
		call2->set_a(a);
		call2->set_op(op2);
		call2->set_a1(a1);
		call2->set_a2(GetIntegerFromAddress(t));
		return true;
	}

	void ReportTruncatedLine();

	Tokenizer lexer_;
};

ParserImpl::ParserImpl(const char *input)
	: lexer_(input)
{
	assert(input);
}

bool ParserImpl::Parse(Body *body)
{
	Token t;
	while ( int r = lexer_(&t) ) {
		if (r < 0)
			return false;
		if (t.type != Token::Type::kSpace) {
			std::cerr << "line starting with non-space: ";
			t.Write(std::cerr) << std::endl;
			return false;
		}
		switch (lexer_(&t)) {
		case 1:
			break;
		case 0:
			ReportTruncatedLine();
			return false;
		default:
			return false;
		}
		switch (t.type) {
		case Token::Type::kSpace:
			{
				bc::Code code;
				if (!ParseInst(&code))
					return false;
				body->code.push_back(code);
			}
			break;
		case Token::Type::kLabel:
			if (!ParseLabelColon(GetIntegerFromLabel(t), body))
				return false;
			break;
		default:
			ReportUnexpectedToken(t);
			return false;
		}
		if (!ReadToken(Token::Type::kEol))
			return false;
	}
	return true;
}

bool ParserImpl::ParseInst(bc::Code *code)
{
	Token t;
	switch (lexer_(&t)) {
	case 1:
		break;
	case 0:
		ReportTruncatedLine();
		return false;
	default:
		return false;
	}
	switch (t.type) {
	case Token::Type::kAddress:
		return ParseInstOp(GetIntegerFromAddress(t), code);
	case Token::Type::kBr:
		return ParseInstBr(code);
	case Token::Type::kJmp:
		return ParseInstJmp(code);
	case Token::Type::kLb:
		return ParseInstLb(code);
	case Token::Type::kLc:
		return ParseInstLc(code);
	case Token::Type::kLd:
		return ParseInstLd(code);
	case Token::Type::kLoad:
		return ParseInstLoad(code);
	case Token::Type::kLoadi:
		return ParseInstLoadi(code);
	case Token::Type::kRet:
		code->set_type(bc::Code::kRet);
		return true;
	case Token::Type::kStore:
		return ParseInstStore(code);
	case Token::Type::kRefer:
		return ParseInstRefer(code);
	case Token::Type::kDeref:
		return ParseInstDeref(code);
	case Token::Type::kAlloc:
		return ParseInstAlloc(code);
	case Token::Type::kSave:
		return ParseInstSave(code);
	case Token::Type::kMove:
		return ParseInstMove(code);
	case Token::Type::kTranspose:
		return ParseInstTranspose(code);
	case Token::Type::kOuterproduct:
		return ParseInstOuterproduct(code);
	case Token::Type::kScalarproduct:
		return ParseInstScalarproduct(code);
	case Token::Type::kVectorproduct:
		return ParseInstVectorproduct(code);
	case Token::Type::kDeterminant:
		return ParseInstDeterminant(code);
	case Token::Type::kSelect2:
		return ParseInstSelect2(code);
	case Token::Type::kSelect3:
		return ParseInstSelect3(code);
	case Token::Type::kSelrow:
		return ParseInstSelrow(code);
	case Token::Type::kMult:
		return ParseInstMult(code);
	case Token::Type::kMmul:
		return ParseInstMmul(code);
	default:
		break;
	}
	ReportUnexpectedToken(t);
	return false;
}

bool ParserImpl::ParseLabelColon(int n, Body *body)
{
	assert(n >= 0);
	if (body->labels.size() <= static_cast<size_t>(n))
		body->labels.resize(n + 1);
	body->labels.at(n) = static_cast<int>(body->code.size());
	return ReadToken(Token::Type::kColon);
}

bool ParserImpl::ParseInstOp(int a, bc::Code *code)
{
	if ( !SkipSpace() ||
		 !ReadToken(Token::Type::kEqualSign) ||
		 !SkipSpace() ||
		 !ReadToken(Token::Type::kParenOpen) )
		return false;
	Token t;
	switch (lexer_(&t)) {
	case 1:
		break;
	case 0:
		ReportTruncatedLine();
		return false;
	default:
		return false;
	}
	switch (t.type) {
	case Token::Type::kAbs:
		return ParseCall1(a, bc::Call1::kAbs, code);
	case Token::Type::kArccos:
		return ParseCall1(a, bc::Call1::kArccos, code);
	case Token::Type::kArccosh:
		return ParseCall1(a, bc::Call1::kArccosh, code);
	case Token::Type::kArccot:
		return ParseCall1(a, bc::Call1::kArccot, code);
	case Token::Type::kArccoth:
		return ParseCall1(a, bc::Call1::kArccoth, code);
	case Token::Type::kArccsc:
		return ParseCall1(a, bc::Call1::kArccsc, code);
	case Token::Type::kArccsch:
		return ParseCall1(a, bc::Call1::kArccsch, code);
	case Token::Type::kArcsec:
		return ParseCall1(a, bc::Call1::kArcsec, code);
	case Token::Type::kArcsech:
		return ParseCall1(a, bc::Call1::kArcsech, code);
	case Token::Type::kArcsin:
		return ParseCall1(a, bc::Call1::kArcsin, code);
	case Token::Type::kArcsinh:
		return ParseCall1(a, bc::Call1::kArcsinh, code);
	case Token::Type::kArctan:
		return ParseCall1(a, bc::Call1::kArctan, code);
	case Token::Type::kArctanh:
		return ParseCall1(a, bc::Call1::kArctanh, code);
	case Token::Type::kCeiling:
		return ParseCall1(a, bc::Call1::kCeiling, code);
	case Token::Type::kCos:
		return ParseCall1(a, bc::Call1::kCos, code);
	case Token::Type::kCosh:
		return ParseCall1(a, bc::Call1::kCosh, code);
	case Token::Type::kCot:
		return ParseCall1(a, bc::Call1::kCot, code);
	case Token::Type::kCoth:
		return ParseCall1(a, bc::Call1::kCoth, code);
	case Token::Type::kCsc:
		return ParseCall1(a, bc::Call1::kCsc, code);
	case Token::Type::kCsch:
		return ParseCall1(a, bc::Call1::kCsch, code);
	case Token::Type::kExp:
		return ParseCall1(a, bc::Call1::kExp, code);
	case Token::Type::kFloor:
		return ParseCall1(a, bc::Call1::kFloor, code);
	case Token::Type::kFactorial:
		return ParseCall1(a, bc::Call1::kFactorial, code);
	case Token::Type::kLn:
		return ParseCall1(a, bc::Call1::kLn, code);
	case Token::Type::kLog10:
		return ParseCall1(a, bc::Call1::kLog10, code);
	case Token::Type::kMinus: // 1 or 2 args
		return ParseCall<bc::Call1::kMinus1, bc::Call2::kMinus2>(a, code);
	case Token::Type::kRoot: // 1 or 2 args
		return ParseCall<bc::Call1::kRoot1, bc::Call2::kRoot2>(a, code);
	case Token::Type::kSec:
		return ParseCall1(a, bc::Call1::kSec, code);
	case Token::Type::kSech:
		return ParseCall1(a, bc::Call1::kSech, code);
	case Token::Type::kSin:
		return ParseCall1(a, bc::Call1::kSin, code);
	case Token::Type::kSinh:
		return ParseCall1(a, bc::Call1::kSinh, code);
	case Token::Type::kTan:
		return ParseCall1(a, bc::Call1::kTan, code);
	case Token::Type::kTanh:
		return ParseCall1(a, bc::Call1::kTanh, code);
	// Call2
	case Token::Type::kDivide:
		return ParseCall2(a, bc::Call2::kDivide, code);
	case Token::Type::kEq:
		return ParseCall2(a, bc::Call2::kEq, code);
	case Token::Type::kGeq:
		return ParseCall2(a, bc::Call2::kGeq, code);
	case Token::Type::kGt:
		return ParseCall2(a, bc::Call2::kGt, code);
	case Token::Type::kLeq:
		return ParseCall2(a, bc::Call2::kLeq, code);
	case Token::Type::kLog:
		return ParseCall2(a, bc::Call2::kLog, code);
	case Token::Type::kLt:
		return ParseCall2(a, bc::Call2::kLt, code);
	case Token::Type::kMax:
		return ParseCall2(a, bc::Call2::kMax, code);
	case Token::Type::kMin:
		return ParseCall2(a, bc::Call2::kMin, code);
	case Token::Type::kNeq:
		return ParseCall2(a, bc::Call2::kNeq, code);
	case Token::Type::kPlus:
		return ParseCall2(a, bc::Call2::kPlus, code);
	case Token::Type::kPower:
		return ParseCall2(a, bc::Call2::kPower, code);
	case Token::Type::kRem: // remainder
		return ParseCall2(a, bc::Call2::kRemainder, code);
	case Token::Type::kTimes:
		return ParseCall2(a, bc::Call2::kTimes, code);
	case Token::Type::kMod: // Modulo
		return ParseCall2(a, bc::Call2::kModulo, code);
	// Gen1
	case Token::Type::kExponentialVariate:
		return ParseGen1(a, bc::Gen1::kExponentialVariate, code);
	case Token::Type::kPoissonVariate:
		return ParseGen1(a, bc::Gen1::kPoissonVariate, code);
	// Gen2
	case Token::Type::kGammaVariate:
		return ParseGen2(a, bc::Gen2::kGammaVariate, code);
	case Token::Type::kGaussVariate:
		return ParseGen2(a, bc::Gen2::kGammaVariate, code);
	case Token::Type::kLognormalVariate:
		return ParseGen2(a, bc::Gen2::kLognormalVariate, code);
	case Token::Type::kUniformVariate:
		return ParseGen2(a, bc::Gen2::kUniformVariate, code);
	case Token::Type::kWeibullVariate:
		return ParseGen2(a, bc::Gen2::kWeibullVariate, code);
	default:
		std::cerr << "unknown instruction: ";
		t.Write(std::cerr) << std::endl;
		return false;
	}
}

bool ParserImpl::ParseCall1(int a, bc::Call1::Op op, bc::Code *code)
{
	int a1;
	if ( ParseFr(&a1) && SkipParenClose() ) {
		code->set_type(bc::Code::kCall1);
		auto *call1 = code->mutable_call1();
		call1->set_a(a);
		call1->set_op(op);
		call1->set_a1(a1);
		return true;
	}
	return false;
}

bool ParserImpl::ParseCall2(int a, bc::Call2::Op op, bc::Code *code)
{
	int a1, a2;
	if ( ParseFr(&a1) && ParseFr(&a2) && SkipParenClose() ) {
		code->set_type(bc::Code::kCall2);
		auto *call2 = code->mutable_call2();
		call2->set_a(a);
		call2->set_op(op);
		call2->set_a1(a1);
		call2->set_a2(a2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseGen1(int a, bc::Gen1::Type type, bc::Code *code)
{
	int a1;
	if ( ParseFr(&a1) && SkipParenClose() ) {
		code->set_type(bc::Code::kGen1);
		auto *gen1 = code->mutable_gen1();
		gen1->set_a(a);
		gen1->set_type(type);
		gen1->set_a1(a1);
		return true;
	}
	return false;
}

bool ParserImpl::ParseGen2(int a, bc::Gen2::Type type, bc::Code *code)
{
	int a1, a2;
	if ( ParseFr(&a1) && ParseFr(&a2) && SkipParenClose() ) {
		code->set_type(bc::Code::kGen2);
		auto *gen2 = code->mutable_gen2();
		gen2->set_a(a);
		gen2->set_type(type);
		gen2->set_a1(a1);
		gen2->set_a2(a2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseFr(int *a)
{
	Token t;
	if ( SkipSpace() && ReadToken(Token::Type::kAddress, &t) ) {
		*a = GetIntegerFromAddress(t);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstBr(bc::Code *code)
{
	int a, n;
	if ( ParseFr(&a) && ParseLabel(&n) ) {
		code->set_type(bc::Code::kBr);
		auto *br = code->mutable_br();
		br->set_a(a);
		br->set_l(n);
		return true;
	}
	return false;
}

bool ParserImpl::ParseLabel(int *n)
{
	Token t;
	if ( SkipSpace() && ReadToken(Token::Type::kLabel, &t) ) {
		*n = GetIntegerFromLabel(t);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstJmp(bc::Code *code)
{
	int n;
	if (ParseLabel(&n)) {
		code->set_type(bc::Code::kJmp);
		auto *jmp = code->mutable_jmp();
		jmp->set_l(n);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstLb(bc::Code *code)
{
	int a, d;
	Token t;
	if ( ParseFr(&a) &&
		 SkipSpace() && ReadIdentifier(&t) &&
		 ParseFr(&d) ) {
		code->set_type(bc::Code::kLb);
		auto *lb = code->mutable_lb();
		lb->set_a(a);
		lb->set_v(t.lexeme, t.size);
		lb->set_d(d);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstLc(bc::Code *code)
{
	int a, i0, d;
	if ( ParseFr(&a) &&
		 SkipSpace() && ReadInteger(&i0) &&
		 ParseFr(&d) ) {
		code->set_type(bc::Code::kLc);
		auto *lc = code->mutable_lc();
		lc->set_a(a);
		lc->set_i0(i0);
		lc->set_d(d);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstLd(bc::Code *code)
{
	int a, i0, i1, d;
	if ( ParseFr(&a) &&
		 SkipSpace() && ReadInteger(&i0) &&
		 SkipSpace() && ReadInteger(&i1) &&
		 ParseFr(&d) ) {
		code->set_type(bc::Code::kLd);
		auto *ld = code->mutable_ld();
		ld->set_a(a);
		ld->set_i0(i0);
		ld->set_i1(i1);
		ld->set_d(d);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstLoad(bc::Code *code)
{
	int a;
	Token t;
	if ( ParseFr(&a) &&
		 SkipSpace() && ReadIdentifier(&t) ) {
		code->set_type(bc::Code::kLoad);
		auto *load = code->mutable_load();
		load->set_a(a);
		load->set_v(t.lexeme, t.size); // TODO
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstLoadi(bc::Code *code)
{
	int a;
	double v;
	if ( ParseFr(&a) && ParseImm(&v) ) {
		code->set_type(bc::Code::kLoadi);
		auto *loadi = code->mutable_loadi();
		loadi->set_a(a);
		loadi->set_v(v);
		return true;
	}
	return false;
}

bool ParserImpl::ParseImm(double *d)
{
	if (!SkipSpace())
		return false;
	Token t;
	if (!ReadToken(&t))
		return false;
	switch (t.type) {
	case Token::Type::kEulergamma:
		*d = boost::math::constants::euler<double>();
		return true;
	case Token::Type::kExponentiale:
		*d = boost::math::constants::e<double>();
		return true;
	case Token::Type::kPi:
		*d = boost::math::constants::pi<double>();
		return true;
	case Token::Type::kTrue:
		*d = 1;
		return true;
	case Token::Type::kFalse:
		*d = 0;
		return true;
	case Token::Type::kNumber:
		*d = std::atof(t.lexeme);
		return true;
	default:
		std::cerr << "immediate value expected, but got: ";
		t.Write(std::cerr) << std::endl;
		return false;
	}
}

bool ParserImpl::ParseInstStore(bc::Code *code)
{
	Token t;
	int a;
	if ( SkipSpace() && ReadIdentifier(&t) &&
		 ParseFr(&a) ) {
		code->set_type(bc::Code::kStore);
		auto *store = code->mutable_store();
		store->set_v(t.lexeme, t.size);
		store->set_a(a);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstRefer(bc::Code *code)
{
	int i0;
	Token t;
	if ( ParseIr(&i0) &&
		 SkipSpace() && ReadIdentifier(&t) ) {
		code->set_type(bc::Code::kRefer);
		auto *x = code->mutable_refer();
		x->set_i0(i0);
		x->set_v(t.lexeme, t.size);
		return true;
	}
	return false;
}

bool ParserImpl::ParseIr(int *i)
{
	Token t;
	if ( SkipSpace() && ReadToken(Token::Type::kIntReg, &t) ) {
		*i = GetIntegerFromIntReg(t);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstDeref(bc::Code *code)
{
	int f0, i1, k;
	if ( ParseFr(&f0) &&
		 ParseIr(&i1) &&
		 SkipSpace() && ReadInteger(&k) ) {
		code->set_type(bc::Code::kDeref);
		auto *x = code->mutable_deref();
		x->set_f0(f0);
		x->set_i1(i1);
		x->set_k(k);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstAlloc(bc::Code *code)
{
	int i0, k;
	if ( ParseIr(&i0) &&
		 SkipSpace() && ReadInteger(&k) ) {
		code->set_type(bc::Code::kAlloc);
		auto *x = code->mutable_alloc();
		x->set_i0(i0);
		x->set_k(k);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstSave(bc::Code *code)
{
	int i1, k;
	Token t;
	if ( SkipSpace() && ReadIdentifier(&t) &&
		 ParseIr(&i1) &&
		 SkipSpace() && ReadInteger(&k) ) {
		code->set_type(bc::Code::kSave);
		auto *x = code->mutable_save();
		x->set_v(t.lexeme, t.size);
		x->set_i1(i1);
		x->set_k(k);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstMove(bc::Code *code)
{
	int i0, f1, k;
	if ( ParseIr(&i0) &&
		 ParseFr(&f1) &&
		 SkipSpace() && ReadInteger(&k) ) {
		code->set_type(bc::Code::kMove);
		auto *x = code->mutable_move();
		x->set_i0(i0);
		x->set_f1(f1);
		x->set_k(k);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstTranspose(bc::Code *code)
{
	int i0, i1, kr, kc;
	if ( ParseIr(&i0) &&
		 ParseIr(&i1) &&
		 SkipSpace() && ReadInteger(&kr) &&
		 SkipSpace() && ReadInteger(&kc) ) {
		code->set_type(bc::Code::kTranspose);
		auto *x = code->mutable_transpose();
		x->set_i0(i0);
		x->set_i1(i1);
		x->set_kc(kc);
		x->set_kr(kr);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstOuterproduct(bc::Code *code)
{
	int i0, k1, i1, k2, i2;
	if ( ParseIr(&i0) &&
		 SkipSpace() && ReadInteger(&k1) &&
		 ParseIr(&i1) &&
		 SkipSpace() && ReadInteger(&k2) &&
		 ParseIr(&i2) ) {
		code->set_type(bc::Code::kOuterproduct);
		auto *x = code->mutable_outerproduct();
		x->set_i0(i0);
		x->set_k1(k1);
		x->set_i1(i1);
		x->set_k2(k2);
		x->set_i2(i2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstScalarproduct(bc::Code *code)
{
	int f0, k, i1, i2;
	if ( ParseFr(&f0) &&
		 SkipSpace() && ReadInteger(&k) &&
		 ParseIr(&i1) &&
		 ParseIr(&i2) ) {
		code->set_type(bc::Code::kScalarproduct);
		auto *x = code->mutable_scalarproduct();
		x->set_f0(f0);
		x->set_k(k);
		x->set_i1(i1);
		x->set_i2(i2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstVectorproduct(bc::Code *code)
{
	int i0, i1, i2;
	if ( ParseIr(&i0) && ParseIr(&i1) && ParseIr(&i2) ) {
		code->set_type(bc::Code::kVectorproduct);
		auto *x = code->mutable_vectorproduct();
		x->set_i0(i0);
		x->set_i1(i1);
		x->set_i2(i2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstDeterminant(bc::Code *code)
{
	int f0, k, i1;
	if ( ParseFr(&f0) &&
		 SkipSpace() && ReadInteger(&k) &&
		 ParseIr(&i1) ) {
		code->set_type(bc::Code::kDeterminant);
		auto *x = code->mutable_determinant();
		x->set_f0(f0);
		x->set_k(k);
		x->set_i1(i1);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstSelect2(bc::Code *code)
{
	int f0, i1, f2;
	if ( ParseFr(&f0) && ParseIr(&i1) && ParseFr(&f2) ) {
		code->set_type(bc::Code::kSelect2);
		auto *x = code->mutable_select2();
		x->set_f0(f0);
		x->set_i1(i1);
		x->set_f2(f2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstSelect3(bc::Code *code)
{
	int f0, kr, kc, i1, f2, f3;
	if ( ParseFr(&f0) &&
		 SkipSpace() && ReadInteger(&kr) &&
		 SkipSpace() && ReadInteger(&kc) &&
		 ParseIr(&i1) &&
		 ParseFr(&f2) &&
		 ParseFr(&f3) ) {
		code->set_type(bc::Code::kSelect3);
		auto *x = code->mutable_select3();
		x->set_f0(f0);
		x->set_kc(kc);
		x->set_kr(kr);
		x->set_i1(i1);
		x->set_f2(f2);
		x->set_f3(f3);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstSelrow(bc::Code *code)
{
	int i0, kr, kc, i1, f2;
	if ( ParseIr(&i0) &&
		 SkipSpace() && ReadInteger(&kr) &&
		 SkipSpace() && ReadInteger(&kc) &&
		 ParseIr(&i1) &&
		 ParseFr(&f2) ) {
		code->set_type(bc::Code::kSelrow);
		auto *x = code->mutable_selrow();
		x->set_i0(i0);
		x->set_kc(kc);
		x->set_kr(kr);
		x->set_i1(i1);
		x->set_f2(f2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstMult(bc::Code *code)
{
	int i0, k, f1, i2;
	if ( ParseIr(&i0) &&
		 SkipSpace() && ReadInteger(&k) &&
		 ParseFr(&f1) &&
		 ParseIr(&i2) ) {
		code->set_type(bc::Code::kMult);
		auto *x = code->mutable_mult();
		x->set_i0(i0);
		x->set_k(k);
		x->set_f1(f1);
		x->set_i2(i2);
		return true;
	}
	return false;
}

bool ParserImpl::ParseInstMmul(bc::Code *code)
{
	int i0, kr, kx, kc, i1, i2;
	if ( ParseIr(&i0) &&
		 SkipSpace() && ReadInteger(&kr) &&
		 SkipSpace() && ReadInteger(&kx) &&
		 SkipSpace() && ReadInteger(&kc) &&
		 ParseIr(&i1) &&
		 ParseIr(&i2) ) {
		code->set_type(bc::Code::kMmul);
		auto *x = code->mutable_mmul();
		x->set_i0(i0);
		x->set_kr(kr);
		x->set_kx(kx);
		x->set_kc(kc);
		x->set_i1(i1);
		x->set_i2(i2);
		return true;
	}
	return false;
}

bool ParserImpl::ReadIdentifier(Token *t)
{
	return ReadToken(Token::Type::kIdentifier, t);
}

bool ParserImpl::ReadInteger(int *i)
{
	Token t;
	if (ReadToken(Token::Type::kNumber, &t)) {
		*i = GetIntegerFromNumber(t);
		return true;
	}
	return false;
}

bool ParserImpl::ReadToken(Token::Type type)
{
	Token t;
	return ReadToken(type, &t);
}

bool ParserImpl::ReadToken(Token::Type type, Token *t)
{
	if (!ReadToken(t))
		return false;
	if (t->type == type)
		return true;
	std::cerr << "expected token of type "
			  << static_cast<int>(type)
			  << ", but got: ";
	t->Write(std::cerr) << std::endl;
	return false;
}

bool ParserImpl::ReadToken(Token *t)
{
	switch (lexer_(t)) {
	case 1:
		return true;
	case 0:
		ReportTruncatedLine();
		return false;
	default:
		return false;
	}
}

bool ParserImpl::SkipParenClose()
{
	return ReadToken(Token::Type::kParenClose);
}

bool ParserImpl::SkipSpace()
{
	return ReadToken(Token::Type::kSpace);
}

void ParserImpl::ReportTruncatedLine()
{
	std::cerr << "truncated line: ";
	lexer_.Dump(std::cerr) << std::endl;
}


Parser::Parser(const char *input)
	: impl_(new ParserImpl(input))
{
	assert(input);
}

Parser::~Parser() = default;

bool Parser::operator()(Body *body)
{
	assert(body);
	return impl_->Parse(body);
}

}
}
}
