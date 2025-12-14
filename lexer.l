%option noyymore noinput nounput
%option yylineno
%{
#include "include/ast.h"
#include "include/errorReporter.h"
#include "parser.tab.hpp"
#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
int yycolumn = 1;
extern bool hasError;

void updateLocationAndColumn() 
{
    yylloc.first_line = yylineno;
    yylloc.first_column = yycolumn;
    yylloc.last_line = yylineno;
    yylloc.last_column = yycolumn + yyleng - 1;
    yycolumn += yyleng;
}
%}

letter [a-zA-Z]
number [0-9]+
floatnumber (([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+))
ident {letter}({letter}|[0-9])*

%%

"program"   { updateLocationAndColumn(); return PROGRAM; } 
"func"      { updateLocationAndColumn(); return FUNC; }
"main"      { updateLocationAndColumn(); return MAIN; }
"let"       { updateLocationAndColumn(); return LET; }
"if"        { updateLocationAndColumn(); return IF; }
"else"      { updateLocationAndColumn(); return ELSE; }
"while"     { updateLocationAndColumn(); return WHILE; }
"input"     { updateLocationAndColumn(); return INPUT; }
"output"    { updateLocationAndColumn(); return OUTPUT; }
"return"    { updateLocationAndColumn(); return RETURN; }
"null"      { updateLocationAndColumn(); return NULLSIGN; }
"int"       { updateLocationAndColumn(); return INTSIGN; }
"float"     { updateLocationAndColumn(); return FLOATSIGN; }

{floatnumber} { updateLocationAndColumn(); yylval.fnum = atof(yytext); return FLOATNUMBER; }
{number}    { updateLocationAndColumn(); yylval.num = atoi(yytext); return NUMBER; }
{ident}     { updateLocationAndColumn(); yylval.ident = new std::string{ yytext }; return IDENT; }

"=="        { updateLocationAndColumn(); return EQ; }
"!="        { updateLocationAndColumn(); return NEQ; }
"<="        { updateLocationAndColumn(); return LE; }
">="        { updateLocationAndColumn(); return GE; }
"<"         { updateLocationAndColumn(); return LT; }
">"         { updateLocationAndColumn(); return GT; }
"="         { updateLocationAndColumn(); return ASSIGN; }
"&"         { updateLocationAndColumn(); return ANDSIGN; }
"+"         { updateLocationAndColumn(); return PLUS; }
"-"         { updateLocationAndColumn(); return MINUS; }
"*"         { updateLocationAndColumn(); return STAR; }
"/"         { updateLocationAndColumn(); return DIVIDE; }
"%"         { updateLocationAndColumn(); return MOD; }

"("         { updateLocationAndColumn(); return LPAREN; }
")"         { updateLocationAndColumn(); return RPAREN; }
"{"         { updateLocationAndColumn(); return LBRACE; }
"}"         { updateLocationAndColumn(); return RBRACE; }
"["         { updateLocationAndColumn(); return LBRACKET; }
"]"         { updateLocationAndColumn(); return RBRACKET; }
":"         { updateLocationAndColumn(); return COLON; }
";"         { updateLocationAndColumn(); return SEMICOLON; }
","         { updateLocationAndColumn(); return COMMA; }

[ \t]+      { yycolumn += yyleng; }       // 空格和 Tab 只更新列号
\n          { yycolumn = 1; }  // 换行时更新行号，重置列号
"//".*"\n"  { yycolumn = 1; }        // 单行注释也更新列号

.           {
                std::string msg = std::string{"未知字符 '"} + yytext + "'";
                reportErrorAt("词法分析", yylineno, yycolumn, msg);
                yycolumn += yyleng;
                return yytext[0];
          }

%%

int yywrap() { return 1; }