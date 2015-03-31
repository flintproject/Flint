%{
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "cas/sexp.h"

#include "flint-combine-parser.hh"
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
"a" {return 'a';}
"c" {return 'c';}
"o" {return 'o';}

{FLOAT}                       {yylval.d = strtod(yytext, NULL); return REAL;}
{SIGN}?{DIGIT}+               {yylval.i = atoi(yytext); return INTEGER;}
"sbml:"[a-zA-Z_][a-zA-Z_0-9]* {yylval.name = strdup(yytext); return NAME;}
[%@][a-zA-Z_][a-zA-Z_0-9:]*   {yylval.sexp = new Sexp(yytext); return ID;}
[a-zA-Z_][a-zA-Z_0-9]*        {yylval.sexp = new Sexp(yytext); return KEYWORD;}
. {std::fprintf(stderr, "unexpeced input: %s\n", yytext); std::exit(EXIT_FAILURE);}
%%
