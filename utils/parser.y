////////////////////////////////////////////////////////
//声明部分
%{
#include<stdio.h>
#include<stdlib.h>
#include<malloc.h>
#include<memory.h>
#include<string.h>


#define txmax 100     /* 符号表容量 */
#define al 10         /* 标识符的最大长度 */

/* 符号表中的类型 */
enum object {
    constant,
    variable,
    procedure,
};

/* 符号表结构 */
struct tablestruct
{
   char name[al];     /* 名字 */
   enum object kind;  /* 类型：const，var或procedure */
};
struct tablestruct table[txmax]; /* 符号表 */


int tx;         /* 符号表当前尾指针 */
char id[al];

FILE* fin;      /* 输入源文件 */
FILE* foutput;  /* 输出出错示意（如有错） */
char fname[al];
int err;
extern int line;

int yydebug = 0;

void init();
void enter(enum object k);
int position(char *s);
void redirectInput(FILE* fin);
int yyerror(char *s);
int yylex(void);
%}

////////////////////////////////////////////////////////
//辅助定义部分
%union{
char *ident;
int number;
}


%token BEGINSYM CALLSYM CONSTSYM DOSYM ENDSYM IFSYM ODDSYM PROCSYM UNTILSYM WITHSYM
%token READSYM THENSYM VARSYM WHILESYM WRITESYM REPEATSYM FORSYM
%token BECOMES LSS LEQ GTR GEQ PLUS MINUS TIMES SLASH LPAREN RPAREN
%token EQL COMMA PERIOD NEQ SEMICOLON


%token <ident> IDENT
%token <number> NUMBER

%type <number> ident

////////////////////////////////////////////////////////
//规则部分
%%
/* 程序 */
program: block PERIOD ;

/* 分程序 */
block: constdecl vardecl procdecls statement ;

/* 常量声明 */
constdecl: CONSTSYM constlist SEMICOLON | ;

/* 常量声明列表 */
constlist: constdef | constlist COMMA constdef ;

/* 单个常量 */
constdef: IDENT EQL NUMBER
            {
               strcpy(id,$1);
               enter(constant);
            };

/*  变量声明 */
vardecl: VARSYM varlist SEMICOLON | ;

/* 变量声明列表 */
varlist: vardef | varlist COMMA vardef ;

/* 单个变量 */
vardef: IDENT
            {
              strcpy(id, $1);
              enter(variable);
            }
        ;

/*  过程声明 */
procdecls: procdecls procdecl procbody |  ;

/*  过程声明头部 */
procdecl: PROCSYM IDENT SEMICOLON
               {
                 strcpy(id, $2);
	         enter(procedure);
               }
        ;

/*  过程声明主体 */
procbody: block SEMICOLON ;

/*  语句 */
statement: assignmentstm | callstm | compoundstm | ifstm |
           whilestm | readstm | writestm | repeatstm | forstm |;

/*  赋值语句 */
assignmentstm: ident BECOMES expression
               {
                 if ($1 == 0)
                       yyerror("Symbol does not exist");
                 else
                    {
                       if (table[$1].kind != variable)
                           yyerror("Symbol should be a variable");
                    }
               };

/*  调用语句 */
callstm: CALLSYM ident
             {
                 if ($2 == 0)
                       yyerror("Symbol does not exist");
                 else
                    {
                       if (table[$2].kind != procedure)
                           yyerror("Symbol should be a procedure");
                    }
              }
             ;

/* 复合语句 */
compoundstm: BEGINSYM statements ENDSYM ;

/* 一条或多条语句 */
statements: statement | statements SEMICOLON statement ;

/* 条件语句 */
ifstm: IFSYM condition THENSYM statement ;

/* 循环语句 */
whilestm: WHILESYM condition DOSYM statement ;

/* repeat-until */
repeatstm: REPEATSYM statement UNTILSYM condition ;

/* for-with-then */
forstm: FORSYM condition WITHSYM statement THENSYM statement;

/* 读语句 */
readstm: READSYM LPAREN readvarlist RPAREN ;

/* 一个或多个读语句的变量 */
readvarlist: readvar | readvarlist COMMA readvar ;

/* 读语句变量 */
readvar: ident
        {} ;

/* 写语句 */
writestm: WRITESYM LPAREN writeexplist RPAREN ;

/* 一个或多个写语句的表达式 */
writeexplist: expression | writeexplist COMMA expression ;

/* 条件表达式 */
condition: ODDSYM expression
          | expression EQL expression
          | expression NEQ expression
          | expression LSS expression
          | expression LEQ expression
          | expression GTR expression
          | expression GEQ expression
          ;

/* 表达式 */
expression: PLUS term
          | MINUS term
          | term
          | expression PLUS term
          | expression MINUS term
          ;

/* 项 */
term: factor
      | term TIMES factor
      | term SLASH factor
      ;

/* 因子 */
factor: ident
               { if ($1 == 0)
                       yyerror("Symbol does not exist");
                 else
                    {
                       if (table[$1].kind == procedure)
                           yyerror("Symbol should not be a procedure");
                    }
                }
       | NUMBER {}
       | LPAREN expression RPAREN;

ident: IDENT
         {
           $$ = position ($1);
         }
        ;



////////////////////////////////////////////////////////
//程序部分
%%
int yyerror(char *s)
{
	err = err + 1;
        printf("%s in line %d\n", s, line);
	fprintf(foutput, "%s in line %d\n", s, line);
	return 0;
}

void init()
{
	tx = 0;
        err = 0;
}

void enter(enum object k)
{
	tx = tx + 1;
	strcpy(table[tx].name, id);
	table[tx].kind = k;
}

int position(char *s)
{
	int i;
	strcpy(table[0].name, s);
	i = tx;
	while(strcmp(table[i].name, s) != 0)
		i--;
	return i;
}

int main(void)
{
	printf("Input pl/0 file?   ");
	scanf("%s", fname);		/* 输入文件名 */

	if ((fin = fopen(fname, "r")) == NULL)
	{
		printf("Can't open the input file!\n");
		exit(1);
	}
	if ((foutput = fopen("foutput.txt", "w")) == NULL)
        {
		printf("Can't open the output file!\n");
		exit(1);
	}

	redirectInput(fin);
	init();
    yyparse();
	if(err == 0)
	{
		printf("\n===Parsing success!===\n");
		fprintf(foutput, "\n===Parsing success!===\n");
	}
        else
	{
		printf("%d errors in PL/0 program\n", err);
		fprintf(foutput, "%d errors in PL/0 program\n", err);
	}
        fclose(foutput);
	fclose(fin);
	return 0;
}



