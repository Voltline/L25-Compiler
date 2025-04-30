%{
#include "stdio.h"
#include "string.h"
#include "malloc.h"
#include "pl0yacc.tab.h"

int line=1;
void yyerror(char *);
int yywrap(void);
%}

%%
[ \t]	{;}
[\n]	{line++;}
"begin"	{return BEGINSYM;}
"call"	{return CALLSYM;}
"const"	{return CONSTSYM;}
"do"	{return DOSYM;}
"end"	{return ENDSYM;}
"if"	{return IFSYM;}
"odd"	{return ODDSYM;}
"procedure"	{return PROCSYM;}
"read"	{return READSYM;}
"then"	{return THENSYM;}
"var"	{return VARSYM;}
"while"	{return WHILESYM;}
"write"	{return WRITESYM;}
"repeat" {return REPEATSYM;}
"until" {return UNTILSYM;}
"for" {return FORSYM;}
"with" {return WITHSYM;}
[A-Za-z]([A-Za-z]|[0-9])*	{yylval.ident=(char *)malloc(strlen(yytext)+1);
                                 strcpy(yylval.ident,yytext);
                                 return IDENT;
}
[0-9]+	{yylval.number=atoi(yytext);
	 return NUMBER;}
":="	{return BECOMES;}
"<"	{return LSS;}
"<="	{return LEQ;}
">"	{return GTR;}
">="	{return GEQ;}
"+"	{return PLUS;}
"-"	{return MINUS;}
"*"	{return TIMES;}
"/"	{return	SLASH;}
"("	{return	LPAREN;}
")"	{return	RPAREN;}
"="	{return	EQL;}
","	{return COMMA;}
"."	{return PERIOD;}
"#"	{return NEQ;}
";"	{return	SEMICOLON;}
.	{yyerror("Unknown character!");}

%%

void redirectInput(FILE *input)
{
	yyin=input;
}

int yygetchar(void)
{
	int ret;
	ret = getc(yyin);
	printf("%c",ret);
	return ret;
}

int yywrap(void)
{
	return 1;
}