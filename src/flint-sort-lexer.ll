%{
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

#include "cas/sexp.h"

#include "flint-sort-parser.hh"
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
[0-9a-f]{8}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{12} {yylval.uuid = strdup(yytext); return UUID36;}
{FLOAT}                      {yylval.d = strtod(yytext, NULL); return REAL;}
{SIGN}?{DIGIT}+              {yylval.i = atoi(yytext); return INTEGER;}
[%@][a-zA-Z_][a-zA-Z_0-9:#]* {yylval.id = strdup(yytext); return ID;}
[$]?[a-zA-Z_][a-zA-Z_0-9]*      {yylval.keyword = strdup(yytext); return KEYWORD;}
. {std::fprintf(stderr, "unexpeced input: %s\n", yytext); std::exit(EXIT_FAILURE);}
%%
