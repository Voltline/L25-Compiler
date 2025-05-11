%{
#include "ast.h"
#include "parser.tab.hpp"
#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
%}

letter [a-zA-Z]
number [0-9]+
ident {letter}({letter}|[0-9])*

%%
"program"   { return PROGRAM; } 
"func"      { return FUNC; }
"main"      { return MAIN; }
"let"       { return LET; }
"if"        { return IF; }
"else"      { return ELSE; }
"while"     { return WHILE; }
"input"     { return INPUT; }
"output"    { return OUTPUT; }
"return"    { return RETURN; }
"null"      { return NULLSIGN; }
"int"       { return INTSIGN; }

{number}    { yylval.num = atoi(yytext); return NUMBER; }
{ident}     { yylval.ident = new std::string{ yytext }; return IDENT; }

"+"         { return PLUS; }
"-"         { return MINUS; }
"*"         { return STAR; }
"/"         { return DIVIDE; }
"=="        { return EQ; }
"!="        { return NEQ; }
"<"         { return LT; }
"<="        { return LE; }
">"         { return GT; }
">="        { return GE; }
"="         { return ASSIGN; }
"&"         { return ANDSIGN; }

"("         { return LPAREN; }
")"         { return RPAREN; }
"{"         { return LBRACE; }
"}"         { return RBRACE; }
"["         { return LBRACKET; }
"]"         { return RBRACKET; }
":"         { return COLON; }
";"         { return SEMICOLON; }
","         { return COMMA; }

[ \t\n]     {}
"//".*      {} /* 仅支持单行注释 */

.           { return yytext[0]; }
%%
int yywrap() { return 1; }