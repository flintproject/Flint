%{
/* Prologue */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <vector>

#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include "cas/sexp.h"

extern FILE *yyin;
int yylex();
void yyerror(char const *);

using std::fprintf;

namespace {

bool IsAnd(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "and") == 0;
}

bool IsOr(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "or") == 0;
}

bool IsAt(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "$At") == 0;
}

bool IsLookback(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "$lookback") == 0;
}

bool IsPiecewise(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "piecewise") == 0;
}

bool IsPiece(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "piece") == 0;
}

bool IsOtherwise(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "otherwise") == 0;
}

bool IsTrial(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "$trial") == 0;
}

bool IsOutcome(Sexp *sexp)
{
	Sexp *car = sexp->GetCar();
	return car->IsSymbol() && strcmp(car->s(), "$outcome") == 0;
}

struct Context {
	const char *uuid;
	const char *id;
	int avail_n;
	int avail_l;
};

void EmitCode(int n, Sexp *sexp, Context *context);

/*
 * The control should reach the end when the sexp is evaluated to be false.
 *
 * @param n the variable for the resulting boolean value
 * @param l the label to go when the sexp is evaluated to be true
 */
void EmitCondition(int n, int l, Sexp *sexp, Context *context)
{
	if (sexp->IsCons()) {
		if (IsAnd(sexp)) {
			Sexp *cdr = sexp->GetCdr();
			Sexp *cadr = cdr->GetCar();
			Sexp *caddr = cdr->GetCdr()->GetCar();
			int l1 = context->avail_l++;
			int l2 = context->avail_l++;
			EmitCondition(n, l2, cadr, context);
			printf("  jmp L%d\n", l1);
			printf(" L%d:\n", l2);
			EmitCondition(n, l, caddr, context);
			printf(" L%d:\n", l1);
			return;
		} else if (IsOr(sexp)) {
			Sexp *cdr = sexp->GetCdr();
			Sexp *cadr = cdr->GetCar();
			Sexp *caddr = cdr->GetCdr()->GetCar();
			EmitCondition(n, l, cadr, context);
			EmitCondition(n, l, caddr, context);
			return;
		}
	}

	EmitCode(n, sexp, context);
	printf("  br $%d L%d\n", n, l);
}

void EmitAt(int n, Sexp *sexp, Context *context)
{
	Sexp *cdr = sexp->GetCdr();
	assert(cdr);
	size_t len = cdr->GetLength();
	if (len > 3) {
		fprintf(stderr, "error: more than 3 arguments: %s %s\n", context->uuid, context->id);
		exit(EXIT_FAILURE);
	}
	if (len < 3) {
		fprintf(stderr, "error: missing arguments: %s %s\n", context->uuid, context->id);
		exit(EXIT_FAILURE);
	}
	Sexp *cadr = cdr->GetCar();
	if (!cadr->IsInt()) {
		fprintf(stderr, "error: invalid 1st argument of At: %s %s\n", context->uuid, context->id);
		exit(EXIT_FAILURE);
	}
	Sexp *cddr = cdr->GetCdr();
	Sexp *caddr = cddr->GetCar();
	if (!caddr->IsInt()) {
		fprintf(stderr, "error: invalid 2nd argument of At: %s %s\n", context->uuid, context->id);
		exit(EXIT_FAILURE);
	}
	Sexp *cadddr = cddr->GetCdr()->GetCar();
	int m = context->avail_n++;
	EmitCode(m, cadddr, context);
	printf("  ld $%d %d %d $%d\n", n, cadr->i(), caddr->i(), m);
}

void EmitLookback(int n, Sexp *sexp, Context *context)
{
	Sexp *cdr = sexp->GetCdr();
	assert(cdr);
	size_t len = cdr->GetLength();
	if (len > 3) {
		std::fprintf(stderr, "error: more than 2 arguments: %s %s\n", context->uuid, context->id);
		exit(EXIT_FAILURE);
	}
	if (len < 2) {
		std::fprintf(stderr, "error: missing arguments: %s %s\n", context->uuid, context->id);
		exit(EXIT_FAILURE);
	}
	Sexp *cadr = cdr->GetCar();
	if (!cadr->IsSymbol()) {
		std::fprintf(stderr, "error: invalid 1st argument of Delay/DeltaTime: %s %s\n", context->uuid, context->id);
		exit(EXIT_FAILURE);
	}
	Sexp *caddr = cdr->GetCdr()->GetCar();
	int m = context->avail_n++;
	EmitCode(m, caddr, context);
	printf("  lb $%d %s $%d\n", n, cadr->s(), m);
}

void EmitPiecewise(int n, Sexp *sexp, Context *context)
{
	Sexp *cdr = sexp->GetCdr();
	assert(cdr);
	int l = context->avail_l++;
	std::vector<int> v1;
	bool otherwise = false;
	do {
		Sexp *cadr = cdr->GetCar();
		if (IsPiece(cadr)) {
			int l1 = context->avail_l++;
			int m = context->avail_n++;
			EmitCondition(m, l1, cadr->GetCdr()->GetCdr()->GetCar(), context);
			v1.push_back(l1);
		} else if (IsOtherwise(cadr)) {
			otherwise = true;
			EmitCode(n, cadr->GetCdr()->GetCar(), context);
			printf("  jmp L%d\n", l);
		}
		cdr = cdr->GetCdr();
	} while (cdr);
	if (!otherwise) printf("  ret\n");
	cdr = sexp->GetCdr();
	std::vector<int>::const_iterator it = v1.begin();
	do {
		Sexp *cadr = cdr->GetCar();
		if (IsPiece(cadr)) {
			printf(" L%d:\n", *it++);
			EmitCode(n, cadr->GetCdr()->GetCar(), context);
			printf("  jmp L%d\n", l);
		} else if (IsOtherwise(cadr)) {
			/* nothing to do */
		} else {
			assert(false);
		}
		cdr = cdr->GetCdr();
	} while (cdr);
	printf(" L%d:\n", l);
}

void EmitTrial(int n, Sexp *sexp, Context *context)
{
	Sexp *cdr = sexp->GetCdr();
	assert(cdr);
	int l = context->avail_l++;
	int p0 = context->avail_n++;
	int p1 = context->avail_n++;
	int p = context->avail_n++;
	printf("  loadi $%d 0\n", p0);
	printf("  loadi $%d 1\n", p1);
	printf("  $%d = ($uniform_variate $%d $%d)\n", p, p0, p1);
	std::vector<int> v1;
	do {
		Sexp *cadr = cdr->GetCar();
		if (IsOutcome(cadr)) {
			int l1 = context->avail_l++;
			int m0 = context->avail_n++;
			printf("  loadi $%d ", m0);
			cadr->GetCdr()->GetCdr()->GetCar()->Print();
			printf("\n");
			int m1 = context->avail_n++;
			printf("  $%d = (leq $%d $%d)\n", m1, p, m0);
			printf("  br $%d L%d\n", m1, l1);
			v1.push_back(l1);
		} else {
			fprintf(stderr, "error: unexpected child of $trial: %s %s\n", context->uuid, context->id);
			exit(EXIT_FAILURE);
		}
		cdr = cdr->GetCdr();
	} while (cdr);
	printf("  ret\n");
	cdr = sexp->GetCdr();
	std::vector<int>::const_iterator it = v1.begin();
	do {
		Sexp *cadr = cdr->GetCar();
		if (IsOutcome(cadr)) {
			printf(" L%d:\n", *it++);
			EmitCode(n, cadr->GetCdr()->GetCar(), context);
			printf("  jmp L%d\n", l);
		} else {
			assert(false);
		}
		cdr = cdr->GetCdr();
	} while (cdr);
	printf(" L%d:\n", l);
}

void EmitCode(int n, Sexp *sexp, Context *context)
{
	if (sexp->IsSymbol()) {
		const char *s = sexp->s();
		if ( (*s == '%' || *s == '@') && isalpha(s[1]) ) {
			printf("  load $%d %s\n", n, sexp->s());
		} else {
			printf("  loadi $%d %s\n", n, sexp->s());
		}
	} else if (sexp->IsCons()) {
		if (IsAt(sexp)) {
			EmitAt(n, sexp, context);
			return;
		}
		if (IsLookback(sexp)) {
			EmitLookback(n, sexp, context);
			return;
		}
		if (IsPiecewise(sexp)) {
			EmitPiecewise(n, sexp, context);
			return;
		}
		if (IsTrial(sexp)) {
			EmitTrial(n, sexp, context);
			return;
		}

		size_t len = sexp->GetLength();
		if (len > 3) {
			std::fprintf(stderr, "error: more than 2 arguments: %s %s\n", context->uuid, context->id);
			exit(EXIT_FAILURE);
		}
		if (len < 2) {
			std::fprintf(stderr, "error: missing arguments: %s %s\n", context->uuid, context->id);
			exit(EXIT_FAILURE);
		}
		Sexp *cdr = sexp->GetCdr();
		do {
			Sexp *cadr = cdr->GetCar();
			int m = context->avail_n++;
			EmitCode(m, cadr, context);
			delete cadr;
			boost::scoped_array<char> buf(new char[20]);
			sprintf(buf.get(), "$%d", m);
			cdr->SetCar(new Sexp(buf.get()));
			cdr = cdr->GetCdr();
		} while (cdr);
		printf("  $%d = ", n);
		sexp->Print();
		printf("\n");
	} else if (sexp->IsInt() || sexp->IsDouble()) {
		printf("  loadi $%d ", n);
		sexp->Print();
		printf("\n");
	}
}

void EmitCode(const char *uuid, const char *id, Sexp* sexp)
{
	boost::scoped_ptr<Context> context(new Context);
	context->uuid = uuid;
	context->id = id;
	context->avail_n = 1;
	context->avail_l = 0;
	EmitCode(0, sexp, context.get());
	printf("  store %s $0\n", id);
	printf("%s %s %d\n", uuid, id, context->avail_n);
	fflush(stdout);
}

Sexp *ReduceL(const char *op, Sexp *cadr, Sexp *cddr)
{
	if (!cddr) return cadr;
	assert(cddr->IsCons());
	Sexp *caddr = cddr->GetCar();
	Sexp *cdddr = cddr->GetCdr();
	delete cddr;
	return ReduceL(op, Sexp::Apply2(new Sexp(op), cadr, caddr), cdddr);
}

Sexp *ReduceR(const char *op, Sexp *cadr, Sexp *cddr)
{
	if (!cddr) return cadr;
	assert(cddr->IsCons());
	Sexp *caddr = cddr->GetCar();
	Sexp *cdddr = cddr->GetCdr();
	delete cddr;
	return Sexp::Apply2(new Sexp(op), cadr,	ReduceR(op, caddr, cdddr));
}

Sexp *Negate(Sexp *sexp)
{
	assert(sexp->IsCons());
	Sexp *car = sexp->GetCar();
	if (!car->IsSymbol()) {
		return NULL; // FIXME
	}
	const char *s = car->s();

	// in case of compound expression
	if (strcmp(s, "and") == 0) {
		Sexp *cdr = sexp->GetCdr();
		assert(cdr);
		Sexp *cadr = cdr->GetCar();
		Sexp *caddr = cdr->GetCdr()->GetCar();
		delete cdr;
		delete car;
		delete sexp;
		return Sexp::Apply2(new Sexp("or"), Negate(cadr), Negate(caddr));
	} else if (strcmp(s, "or") == 0) {
		Sexp *cdr = sexp->GetCdr();
		assert(cdr);
		Sexp *cadr = cdr->GetCar();
		Sexp *caddr = cdr->GetCdr()->GetCar();
		delete cdr;
		delete car;
		delete sexp;
		return Sexp::Apply2(new Sexp("and"), Negate(cadr), Negate(caddr));
	} else if (strcmp(s, "not") == 0) {
		Sexp *cdr = sexp->GetCdr();
		assert(cdr);
		Sexp *cadr = cdr->GetCar();
		delete cdr;
		delete car;
		delete sexp;
		return cadr;
	}

	// in case of atomic expression
	if (strcmp(s, "eq") == 0) {
		delete car;
		sexp->SetCar(new Sexp("neq"));
		return sexp;
	} else if (strcmp(s, "geq") == 0) {
		delete car;
		sexp->SetCar(new Sexp("lt"));
		return sexp;
	} else if (strcmp(s, "gt") == 0) {
		delete car;
		sexp->SetCar(new Sexp("leq"));
		return sexp;
	} else if (strcmp(s, "leq") == 0) {
		delete car;
		sexp->SetCar(new Sexp("gt"));
		return sexp;
	} else if (strcmp(s, "lt") == 0) {
		delete car;
		sexp->SetCar(new Sexp("geq"));
		return sexp;
	} else if (strcmp(s, "neq") == 0) {
		delete car;
		sexp->SetCar(new Sexp("eq"));
		return sexp;
	}
	return NULL; // FIXME
}

Sexp *Mean(Sexp *cadr, Sexp *cddr)
{
	if (!cddr) return cadr;
	assert(cddr->IsCons());
	int n = cddr->GetLength();
	Sexp *sum = ReduceR("plus", cadr, cddr);
	return Sexp::Apply2(new Sexp("divide"), sum, new Sexp(n+1));
}

} // namespace

%}

/* Bison declarations */
%error-verbose /* obsolete directive standing for "%define parse.error verbose" in Bison 2.7 or later */

%union {
	char *uuid;
	int i;
	double d;
	Sexp *sexp;
}

%token NEWLINE
%token AND
%token EQ
%token GEQ
%token GT
%token LEQ
%token LOG
%token LOGBASE
%token LT
%token MAX
%token MEAN
%token MIN
%token NEQ
%token NOT
%token OR
%token OTHERWISE
%token PIECE
%token PIECEWISE
%token PLUS
%token TIMES
%token XOR

%token UNIFORM_VARIATE

%token <uuid> UUID36
%token <i> INTEGER
%token <d> REAL
%token <sexp> ID
%token <sexp> KEYWORD

%type <sexp> sexp seq0 seq1 pexp pseq0 pseq1 lexp lseq0 lseq1

%%
/* Grammer Rules */

input: head
    | input line
    ;

head: INTEGER NEWLINE {printf("%d\n", $1);}

line: UUID36 ID sexp NEWLINE {EmitCode($1, $2->s(), $3);}
    ;

sexp: REAL {$$ = new Sexp($1);}
    | INTEGER {$$ = new Sexp($1);}
    | ID
    | KEYWORD
    | '(' LOG '(' LOGBASE sexp ')' sexp ')' {$$ = Sexp::Apply2(new Sexp("log"), $5, $7);}
    | '(' LOG sexp ')' {$$ = Sexp::Apply1(new Sexp("log10"), $3);}
    | '(' PIECEWISE pseq1 ')' {$$ = new Sexp(new Sexp("piecewise"), $3);}
    | '(' MAX sexp seq0 ')' {$$ = ReduceR("max", $3, $4);}
    | '(' MEAN sexp seq0 ')' {$$ = Mean($3, $4);}
    | '(' MIN sexp seq0 ')' {$$ = ReduceR("min", $3, $4);}
    | '(' PLUS sexp seq0 ')' {$$ = ReduceR("plus", $3, $4);}
    | '(' TIMES sexp seq0 ')' {$$ = ReduceR("times", $3, $4);}
    | '(' UNIFORM_VARIATE sexp sexp sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("$uniform_variate"), $3, $4);} /* FIXME */
    | '(' KEYWORD seq0 ')' {$$ = new Sexp($2, $3);}
    ;

seq0: /* empty */ {$$ = NULL;}
    | seq1
    ;

seq1: sexp seq0 {$$ = new Sexp($1, $2);}
    ;

pexp: '(' PIECE sexp lexp ')' {$$ = Sexp::Apply2(new Sexp("piece"), $3, $4);}
    | '(' OTHERWISE sexp ')'{$$ = Sexp::Apply1(new Sexp("otherwise"), $3);}
    ;

pseq0: /* empty */ {$$ = NULL;}
    | pseq1
    ;

pseq1: pexp pseq0 {$$ = new Sexp($1, $2);}
    ;

lexp: '(' AND lexp lseq0 ')' {$$ = ReduceL("and", $3, $4);}
    | '(' OR lexp lseq0 ')' {$$ = ReduceL("or", $3, $4);}
    | '(' XOR lexp lseq0 ')' {$$ = ReduceL("neq", $3, $4);} /* logical XOR can be considered as NEQ */
    | '(' NOT lexp ')' {$$ = Negate($3);}
    | '(' EQ sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("eq"), $3, $4);}
    | '(' GEQ sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("geq"), $3, $4);}
    | '(' GT sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("gt"), $3, $4);}
    | '(' LEQ sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("leq"), $3, $4);}
    | '(' LT sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("lt"), $3, $4);}
    | '(' NEQ sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("neq"), $3, $4);}
    ;

lseq0: /* empty */ {$$ = NULL;}
    | lseq1
    ;

lseq1: lexp lseq0 {$$ = new Sexp($1, $2);}
    ;

%%
/* Epilogue */

void yyerror(char const *s)
{
	std::fprintf(stderr, "%s\n", s);
}

int main(void)
{
	return yyparse();
}
