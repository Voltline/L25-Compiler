%{
#include "include/ast.h"
#include "include/symbol.h"
#include <memory>
#include <cstdio>
#include <cstdlib>
#include <string>

int yylex();
void yyerror(const char* msg) { fprintf(stderr, "Error: %s\n", msg); }
extern Program* rootProgram;
extern int yydebug;
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
    }
    | PROGRAM IDENT LBRACE func_def_list MAIN LBRACE stmt_list RBRACE RBRACE
    {
        $$ = new Program(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::move(*$4), std::unique_ptr<StmtList>($7));
        rootProgram = $$;
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
    }
    | FUNC IDENT LPAREN RPAREN LBRACE stmt_list RETURN expr SEMICOLON RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), nullptr, std::unique_ptr<StmtList>($6), std::unique_ptr<Expr>($8));
    }
    ;

nested_func_stmt:
    FUNC IDENT LPAREN param_list RPAREN LBRACE stmt_list RETURN expr SEMICOLON RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::unique_ptr<ParamList>($4), std::unique_ptr<StmtList>($7), std::unique_ptr<Expr>($9));
    }
    | FUNC IDENT LPAREN RPAREN LBRACE stmt_list RETURN expr SEMICOLON RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), nullptr, std::unique_ptr<StmtList>($6), std::unique_ptr<Expr>($8));
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
    }
    | LET IDENT ASSIGN expr
    {
        $$ = new DeclareStmt{
            std::make_unique<IdentExpr>(*$2, TypeInfo{ SymbolKind::Int, {} }), 
            std::unique_ptr<Expr>($4)        
        };
    }
    | LET IDENT COLON type_info
    {
        $$ = new DeclareStmt{
            std::make_unique<IdentExpr>(*$2, *$4),
            nullptr
        };
    }
    | LET IDENT COLON type_info ASSIGN expr
    {
        $$ = new DeclareStmt{
            std::make_unique<IdentExpr>(*$2, *$4),
            std::unique_ptr<Expr>($6)
        };
    }
    ;

assign_stmt:
    IDENT ASSIGN expr
    {
        $$ = new AssignStmt{
            std::unique_ptr<IdentExpr>(new IdentExpr(*$1)), std::unique_ptr<Expr>($3)
        };
    }
    ;

if_stmt:
    IF LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new IfStmt{
            std::unique_ptr<BoolExpr>($3), std::unique_ptr<StmtList>($6), nullptr
        };
    }
    | IF LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
    {
        $$ = new IfStmt{
            std::unique_ptr<BoolExpr>($3), std::unique_ptr<StmtList>($6), std::unique_ptr<StmtList>($10)
        };
    }
    ;

while_stmt:
    WHILE LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new WhileStmt{
            std::unique_ptr<BoolExpr>($3), std::unique_ptr<StmtList>($6)
        };
    }
    ;

func_call:
    IDENT LPAREN RPAREN
    {
        $$ = new FuncCallStmt{
            std::unique_ptr<IdentExpr>(new IdentExpr(*$1)), nullptr
        };
    }
    | IDENT LPAREN arg_list RPAREN
    {
        $$ = new FuncCallStmt{
            std::unique_ptr<IdentExpr>(new IdentExpr(*$1)), std::unique_ptr<ArgList>($3)
        };
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
    }
    ;

output_stmt:
    OUTPUT LPAREN arg_list RPAREN
    {
        $$ = new OutputStmt{ std::unique_ptr<ArgList>($3) };
    } 
    ;

bool_expr:
    expr EQ expr
    {
        $$ = new BoolExpr{
            "==", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
    }
    | expr NEQ expr
    {
        $$ = new BoolExpr{
            "!=", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
    }
    | expr LT expr
    {
        $$ = new BoolExpr{
            "<", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
    }
    | expr LE expr
    {
        $$ = new BoolExpr{
            "<=", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
    }
    | expr GT expr
    {
        $$ = new BoolExpr{
            ">", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
    }
    | expr GE expr
    {
        $$ = new BoolExpr{
            ">=", std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
    }
    ;

expr:
    PLUS term %prec UPLUS
    {
        $$ = new UnaryExpr{
          '+', std::unique_ptr<Expr>($2)
        };
    }
    | MINUS term %prec UMINUS
    {
        $$ = new UnaryExpr{
          '-', std::unique_ptr<Expr>($2)
        };
    }
    | expr PLUS term
    {
        $$ = new BinaryExpr{
            '+', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
    }
    | expr MINUS term
    {
        $$ = new BinaryExpr{
            '-', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
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
    }
    | term DIVIDE factor
    {
        $$ = new BinaryExpr{
            '/', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
        };
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
    }
    | IDENT
    {
        $$ = new IdentExpr(*$1);
        delete $1;
    }
    | LPAREN expr RPAREN
    {
        $$ = $2;
    }
    | func_call
    {
        $$ = new FuncCallExpr{std::unique_ptr<FuncCallStmt>($1)};
    }
    | array_subscript_expr
    {
        $$ = $1;
    }
    ;

array_subscript_expr:
    IDENT LBRACKET dim_list RBRACKET
    {
        $$ = new ArraySubscriptExpr{
            std::make_unique<IdentExpr>(*$1, TypeInfo{ SymbolKind::Array, *$3 }),
            *$3
        };
        delete $1;
        delete $3;
    }
    ;

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