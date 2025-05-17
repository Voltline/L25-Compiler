%locations
%define parse.error verbose
%{
#include "include/ast.h"
#include "include/symbol.h"
#include <memory>
#include <cstdio>
#include <cstdlib>
#include <string>
extern int yydebug;
extern int yylineno;
extern int yycolumn;
extern int yyleng;
extern char* yytext;
extern bool hasError;

int yylex();
void yyerror(const char* s) 
{
    fprintf(stderr,
        "\033[1;31m[语法错误]\033[0m "
        "位于 \033[1;33m第 %d 行, 第 %d 列\033[0m: %s "
        "(near \033[1;34m'%s'\033[0m)\n",
        yylineno, yycolumn, s, yytext);
    hasError = true;
}
extern Program* rootProgram;
%}

%union {
    int num;
    std::string* ident;
    Expr* expr;
    ParamList* paramList; // 函数定义
    Stmt* stmt;
    StmtList* stmtList;
    Func* func;
    Program* program;
    std::vector<std::unique_ptr<Func>>* funcList;
    BoolExpr* boolExpr;
    ArgList* argList; // 函数调用与output共用
    FuncCallStmt* funcCallStmt;
    InputArgList* inputArgList; // input专用
    std::vector<int>* dims; // 数组维度信息
    TypeInfo* typeInfo;  // 类型信息
    Func* nestedFuncStmt; // 嵌套函数定义语句
    std::vector<std::unique_ptr<Expr>>* arraySubscriptList; // 数组访问专用下标列表
}

%debug

%type <program> input

%type <func> func
%type <funcList> func_def_list
%type <paramList> param_list
%type <argList> arg_list
%type <inputArgList> input_arg_list

%type <stmt> stmt
%type <stmtList> stmt_list
%type <stmt> declare_stmt
%type <stmt> assign_stmt
%type <stmt> if_stmt
%type <stmt> while_stmt
%type <stmt> input_stmt
%type <stmt> output_stmt
%type <funcCallStmt> func_call // 特殊处理
%type <nestedFuncStmt> nested_func_stmt

%type <expr> expr
%type <expr> term
%type <expr> factor
%type <expr> array_subscript_expr
%type <arraySubscriptList> array_subscript_list

%type <boolExpr> bool_expr

%type <dims> dim_list
%type <typeInfo> type_info

%token <num> NUMBER
%token <ident> IDENT

%token PROGRAM FUNC MAIN LET IF ELSE WHILE INPUT OUTPUT RETURN NULLSIGN INTSIGN
%token PLUS MINUS STAR DIVIDE EQ NEQ LT LE GT GE ASSIGN ANDSIGN
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COLON SEMICOLON COMMA
%left PLUS MINUS
%left STAR DIVIDE

%token UPLUS UMINUS DEREF
%right UPLUS UMINUS
%right DEREF

%%
input:
    PROGRAM IDENT LBRACE MAIN LBRACE stmt_list RBRACE RBRACE
    {
        $$ = new Program(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::vector<std::unique_ptr<Func>>(), std::unique_ptr<StmtList>($6));
        rootProgram = $$;
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | PROGRAM IDENT LBRACE func_def_list MAIN LBRACE stmt_list RBRACE RBRACE
    {
        $$ = new Program(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::move(*$4), std::unique_ptr<StmtList>($7));
        rootProgram = $$;
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

func_def_list
    : func
    {
        $$ = new std::vector<std::unique_ptr<Func>>();
        $$->push_back(std::unique_ptr<Func>($1));
    }
    | func_def_list func
    {
        $$ = $1;
        $$->push_back(std::unique_ptr<Func>($2));
    }
    ;

func:
    FUNC IDENT LPAREN param_list RPAREN LBRACE stmt_list RETURN expr SEMICOLON RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::unique_ptr<ParamList>($4), std::unique_ptr<StmtList>($7), std::unique_ptr<Expr>($9));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | FUNC IDENT LPAREN RPAREN LBRACE stmt_list RETURN expr SEMICOLON RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), nullptr, std::unique_ptr<StmtList>($6), std::unique_ptr<Expr>($8));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

nested_func_stmt:
    FUNC IDENT LPAREN param_list RPAREN LBRACE stmt_list RETURN expr SEMICOLON RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::unique_ptr<ParamList>($4), std::unique_ptr<StmtList>($7), std::unique_ptr<Expr>($9));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | FUNC IDENT LPAREN RPAREN LBRACE stmt_list RETURN expr SEMICOLON RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), nullptr, std::unique_ptr<StmtList>($6), std::unique_ptr<Expr>($8));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

param_list:
    IDENT
    {
        $$ = new ParamList(std::vector<std::unique_ptr<IdentExpr>>());
        $$->params.push_back(std::make_unique<IdentExpr>(*$1, TypeInfo{ SymbolKind::Int, {} }));
        delete $1;
    }
    | IDENT COLON type_info
    {
        $$ = new ParamList(std::vector<std::unique_ptr<IdentExpr>>());
        $$->params.push_back(std::make_unique<IdentExpr>(*$1, *$3));
        delete $1;
        delete $3;
    }
    | param_list COMMA IDENT
    {
        $$ = $1;
        $$->params.push_back(std::make_unique<IdentExpr>(*$3, TypeInfo{ SymbolKind::Int, {} }));
        delete $3;
    }
    | param_list COMMA IDENT COLON type_info
    {
        $$ = $1;
        $$->params.push_back(std::make_unique<IdentExpr>(*$3, *$5));
        delete $3;
        delete $5;
    }
    ;

input_arg_list:
    IDENT
    {
        $$ = new InputArgList(std::vector<std::unique_ptr<IdentExpr>>());
        $$->idents.push_back(std::make_unique<IdentExpr>(*$1, TypeInfo{ SymbolKind::Int, {} }));
        delete $1;
    }
    | input_arg_list COMMA IDENT
    {
        $$ = $1;
        $$->idents.push_back(std::make_unique<IdentExpr>(*$3, TypeInfo{ SymbolKind::Int, {} }));
        delete $3;
    }
    ;

stmt:
    declare_stmt | assign_stmt | if_stmt | while_stmt | input_stmt | output_stmt
    | func_call
    { // func_call是FuncCallStmt类型，到Stmt要隐式转换一次
        $$ = $1;
    }
    | nested_func_stmt
    {
        $$ = $1;
    }
    ;

stmt_list:
    stmt SEMICOLON
    {
        $$ = new StmtList(std::vector<std::unique_ptr<Stmt>>());
        $$->stmts.push_back(std::unique_ptr<Stmt>($1));
    }
    | stmt_list stmt SEMICOLON
    {
        $$ = $1;
        $$->stmts.push_back(std::unique_ptr<Stmt>($2));
    }
    ;

declare_stmt:
    LET IDENT
    {
        $$ = new DeclareStmt{
            std::make_unique<IdentExpr>(*$2, TypeInfo{ SymbolKind::Int, {} }), 
            nullptr
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | LET IDENT ASSIGN expr
    {
        $$ = new DeclareStmt{
            std::make_unique<IdentExpr>(*$2, TypeInfo{ SymbolKind::Int, {} }), 
            std::unique_ptr<Expr>($4)        
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | LET IDENT COLON type_info
    {
        $$ = new DeclareStmt{
            std::make_unique<IdentExpr>(*$2, *$4),
            nullptr
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | LET IDENT COLON type_info ASSIGN expr
    {
        $$ = new DeclareStmt{
            std::make_unique<IdentExpr>(*$2, *$4),
            std::unique_ptr<Expr>($6)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

assign_stmt:
    IDENT ASSIGN expr
    {
        $$ = new AssignStmt{
            std::make_unique<IdentExpr>(*$1),
            std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | array_subscript_expr ASSIGN expr
    {
        $$ = new AssignStmt{
            std::make_unique<ArraySubscriptExpr>(std::move(static_cast<ArraySubscriptExpr*>($1)->array), std::move(static_cast<ArraySubscriptExpr*>($1)->subscript)),
            std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    ;

if_stmt:
    IF LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new IfStmt{
            std::unique_ptr<BoolExpr>($3), std::unique_ptr<StmtList>($6), nullptr
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | IF LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
    {
        $$ = new IfStmt{
            std::unique_ptr<BoolExpr>($3), std::unique_ptr<StmtList>($6), std::unique_ptr<StmtList>($10)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

while_stmt:
    WHILE LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new WhileStmt{
            std::unique_ptr<BoolExpr>($3), std::unique_ptr<StmtList>($6)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

func_call:
    IDENT LPAREN RPAREN
    {
        $$ = new FuncCallStmt{
            std::unique_ptr<IdentExpr>(new IdentExpr(*$1)), nullptr
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | IDENT LPAREN arg_list RPAREN
    {
        $$ = new FuncCallStmt{
            std::unique_ptr<IdentExpr>(new IdentExpr(*$1)), std::unique_ptr<ArgList>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    ;

arg_list:
    expr 
    {
        $$ = new ArgList(std::vector<std::unique_ptr<Expr>>());
        $$->args.push_back(std::unique_ptr<Expr>($1));
    }
    | arg_list COMMA expr
    {
        $$ = $1;
        $$->args.push_back(std::unique_ptr<Expr>($3));
    }
    ;

input_stmt:
    INPUT LPAREN input_arg_list RPAREN
    {
        $$ = new InputStmt{ std::unique_ptr<InputArgList>($3) };
        $$->lineno = yylineno;
        $$->column = yycolumn - yyleng;
    }
    ;

output_stmt:
    OUTPUT LPAREN arg_list RPAREN
    {
        $$ = new OutputStmt{ std::unique_ptr<ArgList>($3) };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    } 
    ;

bool_expr:
    expr EQ expr
    {
        $$ = new BoolExpr{
            "==", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | expr NEQ expr
    {
        $$ = new BoolExpr{
            "!=", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | expr LT expr
    {
        $$ = new BoolExpr{
            "<", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | expr LE expr
    {
        $$ = new BoolExpr{
            "<=", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | expr GT expr
    {
        $$ = new BoolExpr{
            ">", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | expr GE expr
    {
        $$ = new BoolExpr{
            ">=", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    ;

expr:
    PLUS term %prec UPLUS
    {
        $$ = new UnaryExpr{
          '+', std::unique_ptr<Expr>($2)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | MINUS term %prec UMINUS
    {
        $$ = new UnaryExpr{
          '-', std::unique_ptr<Expr>($2)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | expr PLUS term
    {
        $$ = new BinaryExpr{
            '+', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | expr MINUS term
    {
        $$ = new BinaryExpr{
            '-', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | term
    {
        $$ = $1;
    }
    ;

term:
    term STAR factor
    {
        $$ = new BinaryExpr{
            '*', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | term DIVIDE factor
    {
        $$ = new BinaryExpr{
            '/', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | factor
    {
        $$ = $1;
    }
    ;

factor:
    NUMBER
    {
        $$ = new NumberExpr($1);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | IDENT
    {
        $$ = new IdentExpr(*$1);
        delete $1;
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | LPAREN expr RPAREN
    {
        $$ = $2;
    }
    | func_call
    {
        $$ = new FuncCallExpr{std::unique_ptr<FuncCallStmt>($1)};
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | array_subscript_expr
    {
        $$ = $1;
    }
    ;

array_subscript_expr:
    IDENT LBRACKET array_subscript_list RBRACKET
    {
        $$ = new ArraySubscriptExpr{
            std::make_unique<IdentExpr>(*$1, TypeInfo{ SymbolKind::Array, std::vector<int>() }),
            std::move(*$3)
        };
        delete $1;
        delete $3;
        $$->lineno = @3.first_line;
        $$->column = @3.first_column;
    }
    ;

array_subscript_list:
    expr
    {
        $$ = new std::vector<std::unique_ptr<Expr>>();
        $$->push_back(std::unique_ptr<Expr>($1));
    }
    | array_subscript_list COMMA expr
    {
        $$ = $1;
        $$->push_back(std::unique_ptr<Expr>($3));
    }

dim_list:
    NUMBER
    {
        $$ = new std::vector<int>();
        $$->push_back($1);
    }
    | dim_list COMMA NUMBER
    {
        $$ = $1;
        $$->push_back($3);
    }
    ;

type_info:
    INTSIGN
    {
        $$ = new TypeInfo{ SymbolKind::Int, {} };
    }
    |
    LBRACKET dim_list RBRACKET
    {
        $$ = new TypeInfo{ SymbolKind::Array, *$2 };
    }
    ;
%%