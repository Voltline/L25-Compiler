%option noyymore noinput nounput
%option yylineno
%{
#include "ast.h"
#include "errorReporter.h"
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

letter [a-zA-Z_]
number [0-9]+
floatnumber (([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+))
ident {letter}({letter}|[0-9])*

%%

"program"   { updateLocationAndColumn(); return PROGRAM; }
"func"      { updateLocationAndColumn(); return FUNC; }
"main"      { updateLocationAndColumn(); return MAIN; }
"class"     { updateLocationAndColumn(); return CLASS; }
"extends"   { updateLocationAndColumn(); return EXTENDS; }
"let"       { updateLocationAndColumn(); return LET; }
"if"        { updateLocationAndColumn(); return IF; }
"else"      { updateLocationAndColumn(); return ELSE; }
"while"     { updateLocationAndColumn(); return WHILE; }
"for"       { updateLocationAndColumn(); return FOR; }
"break"     { updateLocationAndColumn(); return BREAK; }
"input"     { updateLocationAndColumn(); return INPUT; }
"output"    { updateLocationAndColumn(); return OUTPUT; }
"return"    { updateLocationAndColumn(); return RETURN; }
"this"      { updateLocationAndColumn(); return THIS; }
"new"       { updateLocationAndColumn(); return NEW; }
"delete"    { updateLocationAndColumn(); return DELETE; }
"nil"       { updateLocationAndColumn(); return NIL; }
"null"      { updateLocationAndColumn(); return NIL; }
"int"       { updateLocationAndColumn(); return INTSIGN; }
"float"     { updateLocationAndColumn(); return FLOATSIGN; }
"string"    { updateLocationAndColumn(); return STRINGSIGN; }
"strlen"    { updateLocationAndColumn(); return STRLEN; }
"typename"  { updateLocationAndColumn(); return TYPENAME_KW; }
"fieldcount" { updateLocationAndColumn(); return FIELDCOUNT; }
"methodcount" { updateLocationAndColumn(); return METHODCOUNT; }
"fieldname" { updateLocationAndColumn(); return FIELDNAME; }
"methodname" { updateLocationAndColumn(); return METHODNAME; }
"invoke"  { updateLocationAndColumn(); return INVOKE; }
"vector"    { updateLocationAndColumn(); return VECTOR; }
"map"       { updateLocationAndColumn(); return MAP; }
"deque"     { updateLocationAndColumn(); return DEQUE; }
"queue"     { updateLocationAndColumn(); return QUEUE; }
"channel"   { updateLocationAndColumn(); return CHANNEL; }
"spawn"     { updateLocationAndColumn(); return SPAWN; }
"in"        { updateLocationAndColumn(); return IN; }
"printf"    { updateLocationAndColumn(); return PRINTF; }
"scanf"     { updateLocationAndColumn(); return SCANF; }

{floatnumber} { updateLocationAndColumn(); yylval.fnum = atof(yytext); return FLOATNUMBER; }
{number}    { updateLocationAndColumn(); yylval.num = atoi(yytext); return NUMBER; }

\"([^\"\\]|\\.)*\"  {
                updateLocationAndColumn();
                std::string raw(yytext + 1, yyleng - 2);
                std::string processed;
                for (size_t i = 0; i < raw.size(); ++i) {
                    if (raw[i] == '\\' && i + 1 < raw.size()) {
                        ++i;
                        switch (raw[i]) {
                            case 'n': processed += '\n'; break;
                            case 't': processed += '\t'; break;
                            case 'r': processed += '\r'; break;
                            case '\\': processed += '\\'; break;
                            case '\"': processed += '\"'; break;
                            case '0': processed += '\0'; break;
                            default: processed += raw[i]; break;
                        }
                    } else {
                        processed += raw[i];
                    }
                }
                yylval.strval = new std::string(processed);
                return STRING_LITERAL;
            }

{ident}     { updateLocationAndColumn(); yylval.ident = new std::string{ yytext }; return IDENT; }

"&&"        { updateLocationAndColumn(); return AND; }
"||"        { updateLocationAndColumn(); return OR; }
"!"         { updateLocationAndColumn(); return NOT; }
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
"~"         { updateLocationAndColumn(); return TILDE; }

"("         { updateLocationAndColumn(); return LPAREN; }
")"         { updateLocationAndColumn(); return RPAREN; }
"{"         { updateLocationAndColumn(); return LBRACE; }
"}"         { updateLocationAndColumn(); return RBRACE; }
"["         { updateLocationAndColumn(); return LBRACKET; }
"]"         { updateLocationAndColumn(); return RBRACKET; }
":"         { updateLocationAndColumn(); return COLON; }
"->"        { updateLocationAndColumn(); return ARROW; }
";"         { updateLocationAndColumn(); return SEMICOLON; }
","         { updateLocationAndColumn(); return COMMA; }
"."         { updateLocationAndColumn(); return DOT; }

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