%{
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "cas/sexp.h"

#include "flint-rk4-parser.hh"
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
"case-set" {yylval.sexp = new Sexp(yytext); return CASE_SET;}
"case" {yylval.sexp = new Sexp(yytext); return CASE;}
"condition" {yylval.sexp = new Sexp(yytext); return CONDITION;}
"diff" {yylval.sexp = new Sexp(yytext); return DIFF;}
"eq" {yylval.sexp = new Sexp(yytext); return EQ;}
"$Delay" {return DELAY;}
"$DeltaTime" {return DELTATIME;}
[0-9a-f]{8}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{12} {yylval.uuid = strdup(yytext); return UUID36;}
{FLOAT}                    {yylval.sexp = new Sexp(strtod(yytext, NULL)); return REAL;}
{SIGN}?{DIGIT}+            {yylval.sexp = new Sexp(atoi(yytext)); return INTEGER;}
"%"[a-zA-Z_][a-zA-Z_0-9:]* {yylval.sexp = new Sexp(yytext); return ID;}
[$]?[a-zA-Z_][a-zA-Z_0-9]*    {yylval.sexp = new Sexp(yytext); return KEYWORD;}
. {std::fprintf(stderr, "unexpeced input: %s\n", yytext); std::exit(EXIT_FAILURE);}
%%
