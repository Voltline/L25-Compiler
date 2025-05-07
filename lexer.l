%{
#include "ast.h"
#include "parser.tab.hpp"
#include <cstdio>
#include <cstdlib>
%}

%%

[0-9]+      { yylval.num = atoi(yytext); return NUMBER; }
"+"         { return PLUS; }
"-"         { return MINUS; }
"*"         { return MULTIPLY; }
"/"         { return DIVIDE; }
[ \t\n]     ;  // 忽略空白
.           { return yytext[0]; }

%%
int yywrap() { return 1; }