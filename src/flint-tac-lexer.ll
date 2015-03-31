%{
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "cas/sexp.h"

#include "flint-tac-parser.hh"
%}

%option warn
%option never-interactive
%option nodefault
%option nounistd
%option nounput
%option noyywrap

/* Definitions */
DIGIT [0-9]
SIGN [-+]
EXPONENT [eE]{SIGN}?{DIGIT}+
FLOAT {SIGN}?({DIGIT}*"."{DIGIT}+{EXPONENT}?|{DIGIT}+{EXPONENT})
%%

" "+ { /* skip spaces */ }
"\n" {return NEWLINE;}
"\r" { /* skip cr */ }
"(" {return '(';}
")" {return ')';}
"and"  {return AND;}
"eq"   {return EQ;}
"geq"  {return GEQ;}
"gt"   {return GT;}
"leq"  {return LEQ;}
"log"  {return LOG;}
"logbase" {return LOGBASE;}
"lt"   {return LT;}
"max"  {return MAX;}
"mean" {return MEAN;}
"min"  {return MIN;}
"neq"  {return NEQ;}
"not"  {return NOT;}
"or"   {return OR;}
"otherwise" {return OTHERWISE;}
"piece"     {return PIECE;}
"piecewise" {return PIECEWISE;}
"plus"  {return PLUS;}
"times" {return TIMES;}
"xor"   {return XOR;}

"$uniform_variate" {return UNIFORM_VARIATE;}

[0-9a-f]{8}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{12} {yylval.uuid = strdup(yytext); return UUID36;}
{FLOAT}                      {yylval.d = strtod(yytext, NULL); return REAL;}
{SIGN}?{DIGIT}+              {yylval.i = atoi(yytext); return INTEGER;}
[%@][a-zA-Z_][a-zA-Z_0-9:#]* {yylval.sexp = new Sexp(yytext); return ID;}
[$]?[a-zA-Z_][a-zA-Z_0-9]*      {yylval.sexp = new Sexp(yytext); return KEYWORD;}
. {std::fprintf(stderr, "unexpeced input: %s\n", yytext); std::exit(EXIT_FAILURE);}
%%
