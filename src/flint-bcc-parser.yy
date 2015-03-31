%{
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/* Prologue */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <vector>

#include <boost/math/constants/constants.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include "flint-bcc-block.h"
#include "bc.pb.h"
#include "bc/binary.h"
#include "bc/pack.h"

extern FILE *yyin;
int yylex();
void yyerror(char const *);

using std::cout;
using std::make_pair;
using std::map;
using std::pair;
using std::string;
using std::vector;
using bc::Code;

namespace {

Code *CreateCall1(int address, bc::Call1::Op op, int a1)
{
	Code *c = new Code;
	c->set_type(Code::kCall1);
	bc::Call1 *call1 = c->mutable_call1();
	call1->set_a(address);
	call1->set_op(op);
	call1->set_a1(a1);
	return c;
}

Code *CreateCall2(int address, bc::Call2::Op op, int a1, int a2)
{
	Code *c = new Code;
	c->set_type(Code::kCall2);
	bc::Call2 *call2 = c->mutable_call2();
	call2->set_a(address);
	call2->set_op(op);
	call2->set_a1(a1);
	call2->set_a2(a2);
	return c;
}

Code *CreateLb(int address, const char *name, int d)
{
	Code *c = new Code;
	c->set_type(Code::kLb);
	bc::Lb *lb = c->mutable_lb();
	lb->set_a(address);
	lb->set_v(name);
	lb->set_d(d);
	return c;
}

Code *CreateLd(int address, int i0, int i1, int d)
{
	Code *c = new Code;
	c->set_type(Code::kLd);
	bc::Ld *ld = c->mutable_ld();
	ld->set_a(address);
	ld->set_i0(i0);
	ld->set_i1(i1);
	ld->set_d(d);
	return c;
}

Code *CreateGen1(int address, bc::Gen1::Type type, int a1)
{
	Code *c = new Code;
	c->set_type(Code::kGen1);
	bc::Gen1 *gen1 = c->mutable_gen1();
	gen1->set_a(address);
	gen1->set_type(type);
	gen1->set_a1(a1);
	return c;
}

Code *CreateGen2(int address, bc::Gen2::Type type, int a1, int a2)
{
	Code *c = new Code;
	c->set_type(Code::kGen2);
	bc::Gen2 *gen2 = c->mutable_gen2();
	gen2->set_a(address);
	gen2->set_type(type);
	gen2->set_a1(a1);
	gen2->set_a2(a2);
	return c;
}

Code *CreateBr(int address, int label)
{
	Code *c = new Code;
	c->set_type(Code::kBr);
	bc::Br *br = c->mutable_br();
	br->set_a(address);
	br->set_l(label);
	return c;
}

Code *CreateJmp(int label)
{
	Code *c = new Code;
	c->set_type(Code::kJmp);
	bc::Jmp *jmp = c->mutable_jmp();
	jmp->set_l(label);
	return c;
}

Code *CreateLoad(int address, const char *name)
{
	Code *c = new Code;
	c->set_type(Code::kLoad);
	bc::Load *load = c->mutable_load();
	load->set_a(address);
	load->set_v(name); // TODO
	return c;
}

Code *CreateLoadi(int address, int i)
{
	Code *c = new Code;
	c->set_type(Code::kLoadi);
	bc::Loadi *loadi = c->mutable_loadi();
	loadi->set_a(address);
	loadi->set_v(static_cast<double>(i)); // TODO
	return c;
}

Code *CreateLoadi(int address, double d)
{
	Code *c = new Code;
	c->set_type(Code::kLoadi);
	bc::Loadi *loadi = c->mutable_loadi();
	loadi->set_a(address);
	loadi->set_v(d);
	return c;
}

Code *CreateRet()
{
	Code *c = new Code;
	c->set_type(Code::kRet);
	return c;
}

Code *CreateStore(const char *name, int address)
{
	Code *c = new Code;
	c->set_type(Code::kStore);
	bc::Store *store = c->mutable_store();
	store->set_v(name);
	store->set_a(address);
	return c;
}

typedef boost::ptr_vector<Block> BlockVector;

BlockVector *GetBlockVector()
{
	static boost::scoped_ptr<BlockVector> bv(new BlockVector);
	return bv.get();
}

void InsertBlock(Block *block)
{
	GetBlockVector()->push_back(block);
}

typedef std::vector<std::pair<string, int> > NobVector;

} // namespace

static int nol;

%}

/* Bison declarations */
%error-verbose /* obsolete directive standing for "%define parse.error verbose" in Bison 2.7 or later */

%union {
	char *uuid;
	int i;
	double d;
	char *id;
	int a;
	int l;
	bc::Code *code;
	Block *block;
}

%token NEWLINE
%token BR
%token JMP
%token LB
%token LD
%token LOAD
%token LOADI
%token RET
%token STORE

%token ABS
%token ARCCOS
%token ARCCOSH
%token ARCCOT
%token ARCCOTH
%token ARCCSC
%token ARCCSCH
%token ARCSEC
%token ARCSECH
%token ARCSIN
%token ARCSINH
%token ARCTAN
%token ARCTANH
%token CEILING
%token COS
%token COSH
%token COT
%token COTH
%token CSC
%token CSCH
%token DIVIDE
%token EQ
%token EXP
%token FLOOR
%token GEQ
%token GT
%token LEQ
%token LN
%token LOG
%token LOG10
%token LT
%token MAX
%token MIN
%token MINUS
%token NEQ
%token OR
%token PLUS
%token POWER
%token REM
%token ROOT
%token SEC
%token SECH
%token SIN
%token SINH
%token TAN
%token TANH
%token TIMES

%token EXPONENTIAL_VARIATE
%token GAMMA_VARIATE
%token GAUSS_VARIATE
%token POISSON_VARIATE
%token UNIFORM_VARIATE

%token EULERGAMMA
%token EXPONENTIALE
%token PI

%token <uuid> UUID36
%token <i> INTEGER
%token <d> REAL
%token <id> ID
%token <a> ADDRESS
%token <l> LABEL

%type <block> block code0 code1
%type <code> line

%%
/* Grammer Rules */

input: head
    | input block {$2->LookupLabels(); InsertBlock($2);}
    ;

head: INTEGER NEWLINE {nol = $1;}

block: code1 UUID36 ID INTEGER NEWLINE {$1->set_uuid($2); $1->set_name($3); $1->set_nod($4);}
    ;

code1: code0 LABEL ':' NEWLINE {$1->SaveLabel($2);}
    | code0 line NEWLINE {$1->Add($2);}
    ;

code0: /* empty */ {$$ = new Block;}
    | code1
    ;

line: ADDRESS '=' '(' ABS ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kAbs, $5);}
    | ADDRESS '=' '(' ARCCOS ADDRESS ')'         {$$ = CreateCall1($1, bc::Call1::kArccos, $5);}
    | ADDRESS '=' '(' ARCCOSH ADDRESS ')'        {$$ = CreateCall1($1, bc::Call1::kArccosh, $5);}
    | ADDRESS '=' '(' ARCCOT ADDRESS ')'         {$$ = CreateCall1($1, bc::Call1::kArccot, $5);}
    | ADDRESS '=' '(' ARCCOTH ADDRESS ')'        {$$ = CreateCall1($1, bc::Call1::kArccoth, $5);}
    | ADDRESS '=' '(' ARCCSC ADDRESS ')'         {$$ = CreateCall1($1, bc::Call1::kArccsc, $5);}
    | ADDRESS '=' '(' ARCCSCH ADDRESS ')'        {$$ = CreateCall1($1, bc::Call1::kArccsch, $5);}
    | ADDRESS '=' '(' ARCSEC ADDRESS ')'         {$$ = CreateCall1($1, bc::Call1::kArcsec, $5);}
    | ADDRESS '=' '(' ARCSECH ADDRESS ')'        {$$ = CreateCall1($1, bc::Call1::kArcsech, $5);}
    | ADDRESS '=' '(' ARCSIN ADDRESS ')'         {$$ = CreateCall1($1, bc::Call1::kArcsin, $5);}
    | ADDRESS '=' '(' ARCSINH ADDRESS ')'        {$$ = CreateCall1($1, bc::Call1::kArcsinh, $5);}
    | ADDRESS '=' '(' ARCTAN ADDRESS ')'         {$$ = CreateCall1($1, bc::Call1::kArctan, $5);}
    | ADDRESS '=' '(' ARCTANH ADDRESS ')'        {$$ = CreateCall1($1, bc::Call1::kArctanh, $5);}
    | ADDRESS '=' '(' CEILING ADDRESS ')'        {$$ = CreateCall1($1, bc::Call1::kCeiling, $5);}
    | ADDRESS '=' '(' COS ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kCos, $5);}
    | ADDRESS '=' '(' COSH ADDRESS ')'           {$$ = CreateCall1($1, bc::Call1::kCosh, $5);}
    | ADDRESS '=' '(' COT ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kCot, $5);}
    | ADDRESS '=' '(' COTH ADDRESS ')'           {$$ = CreateCall1($1, bc::Call1::kCoth, $5);}
    | ADDRESS '=' '(' CSC ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kCsc, $5);}
    | ADDRESS '=' '(' CSCH ADDRESS ')'           {$$ = CreateCall1($1, bc::Call1::kCsch, $5);}
    | ADDRESS '=' '(' DIVIDE ADDRESS ADDRESS ')' {$$ = CreateCall2($1, bc::Call2::kDivide, $5, $6);}
    | ADDRESS '=' '(' EQ ADDRESS ADDRESS ')'     {$$ = CreateCall2($1, bc::Call2::kEq, $5, $6);}
    | ADDRESS '=' '(' EXP ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kExp, $5);}
    | ADDRESS '=' '(' FLOOR ADDRESS ')'          {$$ = CreateCall1($1, bc::Call1::kFloor, $5);}
    | ADDRESS '=' '(' GEQ ADDRESS ADDRESS ')'    {$$ = CreateCall2($1, bc::Call2::kGeq, $5, $6);}
    | ADDRESS '=' '(' GT ADDRESS ADDRESS ')'     {$$ = CreateCall2($1, bc::Call2::kGt, $5, $6);}
    | ADDRESS '=' '(' LEQ ADDRESS ADDRESS ')'    {$$ = CreateCall2($1, bc::Call2::kLeq, $5, $6);}
    | ADDRESS '=' '(' LN ADDRESS ')'             {$$ = CreateCall1($1, bc::Call1::kLn, $5);}
    | ADDRESS '=' '(' LOG ADDRESS ADDRESS ')'    {$$ = CreateCall2($1, bc::Call2::kLog, $5, $6);}
    | ADDRESS '=' '(' LOG10 ADDRESS ')'          {$$ = CreateCall1($1, bc::Call1::kLog10, $5);}
    | ADDRESS '=' '(' LT ADDRESS ADDRESS ')'     {$$ = CreateCall2($1, bc::Call2::kLt, $5, $6);}
    | ADDRESS '=' '(' MAX ADDRESS ADDRESS ')'    {$$ = CreateCall2($1, bc::Call2::kMax, $5, $6);}
    | ADDRESS '=' '(' MIN ADDRESS ADDRESS ')'    {$$ = CreateCall2($1, bc::Call2::kMin, $5, $6);}
    | ADDRESS '=' '(' MINUS ADDRESS ')'          {$$ = CreateCall1($1, bc::Call1::kMinus1, $5);}
    | ADDRESS '=' '(' MINUS ADDRESS ADDRESS ')'  {$$ = CreateCall2($1, bc::Call2::kMinus2, $5, $6);}
    | ADDRESS '=' '(' NEQ ADDRESS ADDRESS ')'    {$$ = CreateCall2($1, bc::Call2::kNeq, $5, $6);}
    | ADDRESS '=' '(' PLUS ADDRESS ADDRESS ')'   {$$ = CreateCall2($1, bc::Call2::kPlus, $5, $6);}
    | ADDRESS '=' '(' POWER ADDRESS ADDRESS ')'  {$$ = CreateCall2($1, bc::Call2::kPower, $5, $6);}
    | ADDRESS '=' '(' REM ADDRESS ADDRESS ')'    {$$ = CreateCall2($1, bc::Call2::kRemainder, $5, $6);}
    | ADDRESS '=' '(' ROOT ADDRESS ')'           {$$ = CreateCall1($1, bc::Call1::kRoot1, $5);}
    | ADDRESS '=' '(' ROOT ADDRESS ADDRESS ')'   {$$ = CreateCall2($1, bc::Call2::kRoot2, $5, $6);}
    | ADDRESS '=' '(' SEC ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kSec, $5);}
    | ADDRESS '=' '(' SECH ADDRESS ')'           {$$ = CreateCall1($1, bc::Call1::kSech, $5);}
    | ADDRESS '=' '(' SIN ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kSin, $5);}
    | ADDRESS '=' '(' SINH ADDRESS ')'           {$$ = CreateCall1($1, bc::Call1::kSinh, $5);}
    | ADDRESS '=' '(' TAN ADDRESS ')'            {$$ = CreateCall1($1, bc::Call1::kTan, $5);}
    | ADDRESS '=' '(' TANH ADDRESS ')'           {$$ = CreateCall1($1, bc::Call1::kTanh, $5);}
    | ADDRESS '=' '(' TIMES ADDRESS ADDRESS ')'  {$$ = CreateCall2($1, bc::Call2::kTimes, $5, $6);}

    | ADDRESS '=' '(' EXPONENTIAL_VARIATE ADDRESS ')'     {$$ = CreateGen1($1, bc::Gen1::kExponentialVariate, $5);}
    | ADDRESS '=' '(' GAMMA_VARIATE ADDRESS ADDRESS ')'   {$$ = CreateGen2($1, bc::Gen2::kGammaVariate, $5, $6);}
    | ADDRESS '=' '(' GAUSS_VARIATE ADDRESS ADDRESS ')'   {$$ = CreateGen2($1, bc::Gen2::kGaussVariate, $5, $6);}
    | ADDRESS '=' '(' POISSON_VARIATE ADDRESS ')'         {$$ = CreateGen1($1, bc::Gen1::kPoissonVariate, $5);}
    | ADDRESS '=' '(' UNIFORM_VARIATE ADDRESS ADDRESS ')' {$$ = CreateGen2($1, bc::Gen2::kUniformVariate, $5, $6);}

    | BR ADDRESS LABEL      {$$ = CreateBr($2, $3);}
    | JMP LABEL             {$$ = CreateJmp($2);}
    | LB ADDRESS ID ADDRESS {$$ = CreateLb($2, $3, $4);}
    | LD ADDRESS INTEGER INTEGER ADDRESS {$$ = CreateLd($2, $3, $4, $5);}
    | LOAD ADDRESS ID       {$$ = CreateLoad($2, $3);}
    | LOADI ADDRESS INTEGER {$$ = CreateLoadi($2, $3);}
    | LOADI ADDRESS REAL    {$$ = CreateLoadi($2, $3);}
    | LOADI ADDRESS EULERGAMMA   {$$ = CreateLoadi($2, boost::math::constants::euler<double>());}
    | LOADI ADDRESS EXPONENTIALE {$$ = CreateLoadi($2, boost::math::constants::e<double>());}
    | LOADI ADDRESS PI           {$$ = CreateLoadi($2, boost::math::constants::pi<double>());}
    | RET                   {$$ = CreateRet();}
    | STORE ID ADDRESS      {$$ = CreateStore($2, $3);}
    ;

%%
/* Epilogue */

void yyerror(char const *s)
{
	std::fprintf(stderr, "%s\n", s);
}

int main(void)
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	RequestBinaryStdio();

	int r = yyparse();
	if (r != 0) return r;

	boost::scoped_ptr<NobVector> nv(new NobVector);
	for (BlockVector::const_iterator it=GetBlockVector()->begin();it!=GetBlockVector()->end();++it) {
		if (nv->empty()) {
			nv->push_back(make_pair(it->uuid(), 1));
		} else if (nv->back().first == it->uuid()) {
			nv->back().second++;
		} else {
			nv->push_back(make_pair(it->uuid(), 1));
		}
	}
	// write header
	boost::scoped_ptr<bc::Header> header(new bc::Header);
	header->set_nol(nol);
	header->set_nos(static_cast<int>(nv->size()));
	if (!PackToOstream(*header, &cout)) {
		return EXIT_FAILURE;
	}
	// write section headers
	boost::uuids::string_generator gen;
	boost::scoped_array<char> bu(new char[16]); // 16 is UUID's size
	boost::scoped_ptr<bc::SectionHeader> sh(new bc::SectionHeader);
	for (NobVector::const_iterator nit=nv->begin();nit!=nv->end();++nit) {
		boost::uuids::uuid u = gen(nit->first);
		std::copy(u.begin(), u.end(), bu.get());
		sh->set_id(bu.get(), 16);
		sh->set_nob(nit->second);
		if (!PackToOstream(*sh, &cout)) {
			return EXIT_FAILURE;
		}
	}
	// write block headers
	boost::scoped_ptr<bc::BlockHeader> bh(new bc::BlockHeader);
	for (BlockVector::const_iterator it=GetBlockVector()->begin();it!=GetBlockVector()->end();++it) {
		bh->set_name(it->name());
		bh->set_nod(it->nod());
		bh->set_noc(it->GetCodeSize());
		if (!PackToOstream(*bh, &cout)) {
			return EXIT_FAILURE;
		}
	}
	// write body
	for (BlockVector::const_iterator it=GetBlockVector()->begin();it!=GetBlockVector()->end();++it) {
		it->Print();
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
