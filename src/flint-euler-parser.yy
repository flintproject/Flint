%{
/* Prologue */
#include <cstdio>
#include <cstdlib>

#include "cas/ast.h"
#include "cas/sexp.h"
#include "cas/transform.h"

#include <boost/scoped_ptr.hpp>

extern FILE *yyin;
int yylex();
void yyerror(char const *);

EulerTransformer *GetTransformer();

%}

/* Bison declarations */
%error-verbose /* obsolete directive standing for "%define parse.error verbose" in Bison 2.7 or later */

%union {
	char *uuid;
	Sexp *sexp;
}

%token NEWLINE
%token DELAY
%token DELTATIME
%token <sexp> CASE_SET
%token <sexp> CASE
%token <sexp> CONDITION
%token <sexp> DIFF
%token <sexp> EQ

%token <sexp> INTEGER
%token <sexp> REAL
%token <sexp> ID
%token <sexp> KEYWORD
%token <uuid> UUID36

%type <sexp> statement equation conditional lexp cseq0 cseq1 cexp sexp seq0 seq1

%%
/* Grammer Rules */

input: /* empty */
    | input line
    ;

line: UUID36 statement NEWLINE {ProcessLine(GetTransformer(), $1, $2);free($1);}
    ;

statement: equation
    | conditional
    ;

equation: '(' EQ lexp sexp ')' {$$ = Sexp::CreateList3($2, $3, $4);}
    ;

lexp: '(' DIFF sexp ID ')' {$$ = Sexp::CreateList3($2, $3, $4);}
    | ID
    ;

conditional: '(' CASE_SET cseq1 ')' {$$ = new Sexp($2, $3);}
    ;

cseq0: /* empty */ {$$ = NULL;}
    | cseq1
    ;

cseq1: cexp cseq0 {$$ = new Sexp($1, $2);}
    ;

cexp: '(' CASE '(' CONDITION sexp ')' statement ')' {$$ = Sexp::CreateList3($2, $5, $7);}
    | '(' CASE statement ')' {$$ = Sexp::CreateList2($2, $3);}
    ;

sexp: REAL
    | INTEGER
    | ID
    | KEYWORD
    | '(' DELAY sexp sexp ')' {$$ = Sexp::Apply2(new Sexp("$lookback"), $3, Sexp::Apply2(new Sexp("minus"), new Sexp("%time"), $4));}
    | '(' DELTATIME sexp ')' {$$ = Sexp::Apply2(new Sexp("$lookback"), $3, Sexp::Apply2(new Sexp("minus"), new Sexp("%time"), new Sexp("@dt")));}
    | '(' EQ seq0 ')' {$$ = new Sexp($2, $3);}
    | '(' KEYWORD seq0 ')' {$$ = new Sexp($2, $3);}
    ;

seq0: /* empty */ {$$ = NULL;}
    | seq1
    ;

seq1: sexp seq0 {$$ = new Sexp($1, $2);};

%%
/* Epilogue */

void yyerror(char const *s)
{
	std::fprintf(stderr, "%s\n", s);
}

EulerTransformer *GetTransformer()
{
	static boost::scoped_ptr<EulerTransformer> tf(new EulerTransformer);
	return tf.get();
}

int main(void)
{
	printf("1\n"); /* 1 layer */
	return yyparse();
}
