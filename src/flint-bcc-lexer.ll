%{
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "flint-bcc-block.h"
#include "bc.pb.h"

#include "flint-bcc-parser.hh"
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
":" {return ':';}
"=" {return '=';}
"br"    {return BR;}
"jmp"   {return JMP;}
"lb"    {return LB;}
"ld"    {return LD;}
"load"  {return LOAD;}
"loadi" {return LOADI;}
"ret"   {return RET;}
"store" {return STORE;}

"abs"     {return ABS;}
"arccos"  {return ARCCOS;}
"arccosh" {return ARCCOSH;}
"arccot"  {return ARCCOT;}
"arccoth" {return ARCCOTH;}
"arccsc"  {return ARCCSC;}
"arccsch" {return ARCCSCH;}
"arcsec"  {return ARCSEC;}
"arcsech" {return ARCSECH;}
"arcsin"  {return ARCSIN;}
"arcsinh" {return ARCSINH;}
"arctan"  {return ARCTAN;}
"arctanh" {return ARCTANH;}
"ceiling" {return CEILING;}
"cos"     {return COS;}
"cosh"    {return COSH;}
"cot"     {return COT;}
"coth"    {return COTH;}
"csc"     {return CSC;}
"csch"    {return CSCH;}
"divide"  {return DIVIDE;}
"eq"      {return EQ;}
"exp"     {return EXP;}
"floor"   {return FLOOR;}
"geq"     {return GEQ;}
"gt"      {return GT;}
"leq"     {return LEQ;}
"ln"      {return LN;}
"log"     {return LOG;}
"log10"   {return LOG10;}
"lt"      {return LT;}
"max"     {return MAX;}
"min"     {return MIN;}
"minus"   {return MINUS;}
"neq"     {return NEQ;}
"plus"    {return PLUS;}
"power"   {return POWER;}
"rem"     {return REM;}
"root"    {return ROOT;}
"sec"     {return SEC;}
"sech"    {return SECH;}
"sin"     {return SIN;}
"sinh"    {return SINH;}
"tan"     {return TAN;}
"tanh"    {return TANH;}
"times"   {return TIMES;}

"$exponential_variate" {return EXPONENTIAL_VARIATE;}
"$gamma_variate"       {return GAMMA_VARIATE;}
"$gauss_variate"       {return GAUSS_VARIATE;}
"$poisson_variate"     {return POISSON_VARIATE;}
"$uniform_variate"     {return UNIFORM_VARIATE;}

"eulergamma"   {return EULERGAMMA;}
"exponentiale" {return EXPONENTIALE;}
"pi"           {return PI;}

[0-9a-f]{8}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{4}"-"[0-9a-f]{12} {yylval.uuid = strdup(yytext); return UUID36;}
{FLOAT}                     {yylval.d = strtod(yytext, NULL); return REAL;}
{SIGN}?{DIGIT}+             {yylval.i = atoi(yytext); return INTEGER;}
"$"{DIGIT}+                 {yylval.a = atoi(yytext+1); return ADDRESS;}
"L"{DIGIT}+                 {yylval.l = atoi(yytext+1); return LABEL;}
[%@][a-zA-Z_][a-zA-Z_0-9:#]* {yylval.id = strdup(yytext); return ID;}
. {std::fprintf(stderr, "unexpeced input: %s\n", yytext); std::exit(EXIT_FAILURE);}
%%
