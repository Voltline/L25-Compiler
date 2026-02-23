%locations
%define parse.error verbose
%define lr.type ielr
%glr-parser
%expect-rr 2
%code requires {
#include "ast.h"
struct ClassMemberAggregate;
struct SpawnStmt;
struct BreakStmt;
struct ChannelRecvStmt;
struct ForRangeChannelStmt;
}

%{
#include "ast.h"
#include "errorReporter.h"
#include "symbol.h"
#include <memory>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <algorithm>
#include <iterator>
extern int yydebug;
extern int yylineno;
extern int yycolumn;
extern int yyleng;
extern char* yytext;
extern bool hasError;

int yylex();
void yyerror(const char* s)
{
    std::string msg = std::string{s} + " (near '" + yytext + "')";
    reportErrorAt("语法分析", yylineno, yycolumn, msg);
}
struct ClassMemberAggregate {
    std::vector<std::unique_ptr<FieldDecl>> fields;
    std::vector<std::unique_ptr<MethodDecl>> methods;
    std::vector<std::unique_ptr<CtorDecl>> ctors;
    std::unique_ptr<DtorDecl> dtor;
};
extern Program* rootProgram;
%}

%union {
    int num;
    double fnum;
    std::string* ident;
    std::string* strval;
    Expr* expr;
    ParamList* paramList; // 函数定义
    Stmt* stmt;
    StmtList* stmtList;
    Func* func;
    Program* program;
    std::vector<std::unique_ptr<Func>>* funcList;
    ClassDecl* classDecl;
    std::vector<std::unique_ptr<ClassDecl>>* classList;
    FieldDecl* fieldDecl;
    MethodDecl* methodDecl;
    CtorDecl* ctorDecl;
    DtorDecl* dtorDecl;
    ClassMemberAggregate* classMembers;
    EnumDecl* enumDecl;
    std::vector<std::unique_ptr<EnumDecl>>* enumList;
    std::vector<std::string>* enumValues;
    std::vector<std::string>* importList;
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
%type <typeInfo> opt_return_type
%type <classDecl> class_def
%type <classList> class_def_list opt_class_def_list
%type <classMembers> class_member_list class_member
%type <fieldDecl> field_decl
%type <methodDecl> method_decl
%type <ctorDecl> ctor_decl
%type <dtorDecl> dtor_decl
%type <enumDecl> enum_def
%type <enumList> enum_def_list opt_enum_def_list
%type <enumValues> enum_value_list
%type <importList> import_list opt_import_list
%type <paramList> param_list
%type <argList> arg_list
%type <inputArgList> input_arg_list

%type <stmt> stmt
%type <stmtList> stmt_list
%type <stmt> declare_stmt
%type <stmt> assign_stmt
%type <stmt> if_stmt
%type <stmt> while_stmt
%type <stmt> for_stmt
%type <stmt> channel_recv_stmt
%type <stmt> for_range_channel_stmt
%type <stmt> input_stmt
%type <stmt> output_stmt
%type <stmt> printf_stmt
%type <stmt> scanf_stmt
%type <funcCallStmt> func_call // 特殊处理
%type <nestedFuncStmt> nested_func_stmt

%type <expr> expr
%type <expr> term
%type <expr> factor
%type <expr> deref_expr
%type <expr> addr_expr
%type <expr> assignable
%type <expr> array_subscript_expr
%type <arraySubscriptList> array_subscript_list

%type <boolExpr> bool_expr

%type <dims> dim_list
%type <typeInfo> type_info
%type <typeInfo> base_type

%token <num> NUMBER
%token <fnum> FLOATNUMBER
%token <strval> STRING_LITERAL
%token <ident> IDENT

%token PROGRAM FUNC MAIN LET IF ELSE WHILE FOR INPUT OUTPUT RETURN NIL INTSIGN FLOATSIGN STRINGSIGN STRLEN CLASS EXTENDS THIS NEW DELETE BREAK TRUE_KW FALSE_KW ENUM IMPORT
%token TYPENAME_KW FIELDCOUNT METHODCOUNT FIELDNAME METHODNAME INVOKE
%token VECTOR MAP DEQUE QUEUE CHANNEL SPAWN IN
%token PRINTF SCANF
%token ARROW
%token PLUS MINUS STAR DIVIDE EQ NEQ LT LE GT GE ASSIGN ANDSIGN MOD DOT TILDE
%token AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COLON SEMICOLON COMMA
%left OR
%left AND
%right NOT
%left PLUS MINUS
%left STAR DIVIDE
%left DOT

%token UPLUS UMINUS DEREF
%right UPLUS UMINUS
%right DEREF

%%
input:
    PROGRAM IDENT LBRACE opt_import_list opt_enum_def_list opt_class_def_list MAIN LBRACE stmt_list RBRACE RBRACE
    {
        $$ = new Program(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::move(*$4), std::move(*$6), std::move(*$5), std::vector<std::unique_ptr<Func>>(), std::unique_ptr<StmtList>($9));
        rootProgram = $$;
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $4;
        delete $5;
        delete $6;
    }
    | PROGRAM IDENT LBRACE opt_import_list opt_enum_def_list opt_class_def_list func_def_list MAIN LBRACE stmt_list RBRACE RBRACE
    {
        $$ = new Program(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::move(*$4), std::move(*$6), std::move(*$5), std::move(*$7), std::unique_ptr<StmtList>($10));
        rootProgram = $$;
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $4;
        delete $5;
        delete $6;
    }
    ;

opt_import_list
    :
    {
        $$ = new std::vector<std::string>();
    }
    | import_list
    {
        $$ = $1;
    }
    ;

import_list
    : IMPORT IDENT SEMICOLON
    {
        $$ = new std::vector<std::string>();
        $$->push_back(*$2);
        delete $2;
    }
    | import_list IMPORT IDENT SEMICOLON
    {
        $$ = $1;
        $$->push_back(*$3);
        delete $3;
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

opt_class_def_list
    :
    {
        $$ = new std::vector<std::unique_ptr<ClassDecl>>();
    }
    | class_def_list
    {
        $$ = $1;
    }
    ;

opt_enum_def_list
    :
    {
        $$ = new std::vector<std::unique_ptr<EnumDecl>>();
    }
    | enum_def_list
    {
        $$ = $1;
    }
    ;

enum_def_list
    : enum_def
    {
        $$ = new std::vector<std::unique_ptr<EnumDecl>>();
        $$->push_back(std::unique_ptr<EnumDecl>($1));
    }
    | enum_def_list enum_def
    {
        $$ = $1;
        $$->push_back(std::unique_ptr<EnumDecl>($2));
    }
    ;

enum_def
    : ENUM IDENT LBRACE enum_value_list RBRACE
    {
        $$ = new EnumDecl(*$2, std::move(*$4));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
        delete $4;
    }
    ;

enum_value_list
    : IDENT
    {
        $$ = new std::vector<std::string>();
        $$->push_back(*$1);
        delete $1;
    }
    | enum_value_list COMMA IDENT
    {
        $$ = $1;
        $$->push_back(*$3);
        delete $3;
    }
    ;

class_def_list
    : class_def
    {
        $$ = new std::vector<std::unique_ptr<ClassDecl>>();
        $$->push_back(std::unique_ptr<ClassDecl>($1));
    }
    | class_def_list class_def
    {
        $$ = $1;
        $$->push_back(std::unique_ptr<ClassDecl>($2));
    }
    ;

class_def:
    CLASS IDENT LBRACE class_member_list RBRACE
    {
        $$ = new ClassDecl(std::make_unique<IdentExpr>(*$2), nullptr, std::move($4->fields), std::move($4->methods), std::move($4->ctors), std::move($4->dtor));
        delete $2;
        delete $4;
    }
    | CLASS IDENT EXTENDS IDENT LBRACE class_member_list RBRACE
    {
        $$ = new ClassDecl(std::make_unique<IdentExpr>(*$2), std::make_unique<IdentExpr>(*$4), std::move($6->fields), std::move($6->methods), std::move($6->ctors), std::move($6->dtor));
        delete $2;
        delete $4;
        delete $6;
    }
    ;

class_member_list
    : class_member
    {
        $$ = $1;
    }
    | class_member_list class_member
    {
        $$ = $1;
        $$->fields.reserve($$->fields.size() + $2->fields.size());
        $$->methods.reserve($$->methods.size() + $2->methods.size());
        $$->ctors.reserve($$->ctors.size() + $2->ctors.size());
        if ($2->dtor && $$->dtor) {
            reportErrorAt("语法分析", @1.first_line, @1.first_column, "析构函数重复定义");
        } else if ($2->dtor) {
            $$->dtor = std::move($2->dtor);
        }
        std::move($2->fields.begin(), $2->fields.end(), std::back_inserter($$->fields));
        std::move($2->methods.begin(), $2->methods.end(), std::back_inserter($$->methods));
        std::move($2->ctors.begin(), $2->ctors.end(), std::back_inserter($$->ctors));
        delete $2;
    }
    ;

class_member
    : field_decl SEMICOLON
    {
        $$ = new ClassMemberAggregate();
        $$->fields.push_back(std::unique_ptr<FieldDecl>($1));
    }
    | method_decl
    {
        $$ = new ClassMemberAggregate();
        $$->methods.push_back(std::unique_ptr<MethodDecl>($1));
    }
    | ctor_decl
    {
        $$ = new ClassMemberAggregate();
        $$->ctors.push_back(std::unique_ptr<CtorDecl>($1));
    }
    | dtor_decl
    {
        $$ = new ClassMemberAggregate();
        $$->dtor.reset($1);
    }
    ;

field_decl
    : LET IDENT COLON type_info
    {
        $$ = new FieldDecl(std::make_unique<IdentExpr>(*$2, *$4), *$4);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
        delete $4;
    }
    ;

method_decl
    : FUNC IDENT LPAREN param_list RPAREN opt_return_type LBRACE stmt_list RBRACE
    {
        $$ = new MethodDecl(std::make_unique<IdentExpr>(*$2), std::unique_ptr<ParamList>($4), std::unique_ptr<StmtList>($8), nullptr, *$6);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
        delete $6;
    }
    | FUNC IDENT LPAREN RPAREN opt_return_type LBRACE stmt_list RBRACE
    {
        $$ = new MethodDecl(std::make_unique<IdentExpr>(*$2), nullptr, std::unique_ptr<StmtList>($7), nullptr, *$5);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
        delete $5;
    }
    ;

ctor_decl
    : IDENT LPAREN param_list RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new CtorDecl(std::make_unique<IdentExpr>(*$1), std::unique_ptr<ParamList>($3), std::unique_ptr<StmtList>($6));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $1;
    }
    | IDENT LPAREN RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new CtorDecl(std::make_unique<IdentExpr>(*$1), nullptr, std::unique_ptr<StmtList>($5));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $1;
    }
    ;

dtor_decl
    : TILDE IDENT LPAREN RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new DtorDecl(std::make_unique<IdentExpr>(*$2), std::unique_ptr<StmtList>($6));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
    }
    ;

func:
    FUNC IDENT LPAREN param_list RPAREN opt_return_type LBRACE stmt_list RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::unique_ptr<ParamList>($4), std::unique_ptr<StmtList>($8), nullptr, *$6);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $6;
    }
    | FUNC IDENT LPAREN RPAREN opt_return_type LBRACE stmt_list RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), nullptr, std::unique_ptr<StmtList>($7), nullptr, *$5);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $5;
    }
    ;

nested_func_stmt:
    FUNC IDENT LPAREN param_list RPAREN opt_return_type LBRACE stmt_list RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), std::unique_ptr<ParamList>($4), std::unique_ptr<StmtList>($8), nullptr, *$6);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $6;
    }
    | FUNC IDENT LPAREN RPAREN opt_return_type LBRACE stmt_list RBRACE
    {
        $$ = new Func(std::unique_ptr<IdentExpr>(new IdentExpr(*$2)), nullptr, std::unique_ptr<StmtList>($7), nullptr, *$5);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $5;
    }
    ;

opt_return_type:
    /* empty */
    {
        $$ = new TypeInfo{ SymbolKind::Int, {} };
    }
    | ARROW type_info
    {
        $$ = $2;
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
        $$ = new InputArgList(std::vector<std::unique_ptr<Expr>>());
        $$->idents.push_back(std::make_unique<IdentExpr>(*$1, TypeInfo{ SymbolKind::Int, {} }));
        delete $1;
    }
    | array_subscript_expr
    {
        $$ = new InputArgList(std::vector<std::unique_ptr<Expr>>());
        $$->idents.push_back(std::unique_ptr<Expr>($1));
    }
    | input_arg_list COMMA IDENT
    {
        $$ = $1;
        $$->idents.push_back(std::make_unique<IdentExpr>(*$3, TypeInfo{ SymbolKind::Int, {} }));
        delete $3;
    }
    | input_arg_list COMMA array_subscript_expr
    {
        $$ = $1;
        $$->idents.push_back(std::unique_ptr<Expr>($3));
    }
    ;

stmt:
    declare_stmt | assign_stmt | if_stmt | while_stmt | for_stmt | for_range_channel_stmt | channel_recv_stmt | input_stmt | output_stmt | printf_stmt | scanf_stmt
    | RETURN expr
    {
        $$ = new ReturnStmt(std::unique_ptr<Expr>($2));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | RETURN
    {
        $$ = new ReturnStmt(nullptr);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | BREAK
    {
        $$ = new BreakStmt();
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | func_call
    { // func_call是FuncCallStmt类型，到Stmt要隐式转换一次
        $$ = $1;
    }
    | SPAWN LBRACE stmt_list RBRACE
    {
        $$ = new SpawnStmt(std::unique_ptr<StmtList>($3));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | nested_func_stmt
    {
        $$ = $1;
    }
    | factor DOT IDENT LPAREN RPAREN
    {
        auto* methodCall = new MethodCallExpr(std::unique_ptr<Expr>($1), std::make_unique<IdentExpr>(*$3), nullptr);
        methodCall->lineno = @2.first_line;
        methodCall->column = @2.first_column;
        $$ = new ExprStmt(std::unique_ptr<Expr>(methodCall));
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
        delete $3;
    }
    | factor DOT IDENT LPAREN arg_list RPAREN
    {
        auto* methodCall = new MethodCallExpr(std::unique_ptr<Expr>($1), std::make_unique<IdentExpr>(*$3), std::unique_ptr<ArgList>($5));
        methodCall->lineno = @2.first_line;
        methodCall->column = @2.first_column;
        $$ = new ExprStmt(std::unique_ptr<Expr>(methodCall));
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
        delete $3;
    }
    | DELETE expr
    {
        $$ = new DeleteStmt(std::unique_ptr<Expr>($2));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | INVOKE LPAREN expr COMMA expr RPAREN
    {
        auto* invokeExpr = new InvokeExpr(std::unique_ptr<Expr>($3), std::unique_ptr<Expr>($5), nullptr);
        invokeExpr->lineno = @1.first_line;
        invokeExpr->column = @1.first_column;
        $$ = new ExprStmt(std::unique_ptr<Expr>(invokeExpr));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | INVOKE LPAREN expr COMMA expr COMMA arg_list RPAREN
    {
        auto* invokeExpr = new InvokeExpr(std::unique_ptr<Expr>($3), std::unique_ptr<Expr>($5), std::unique_ptr<ArgList>($7));
        invokeExpr->lineno = @1.first_line;
        invokeExpr->column = @1.first_column;
        $$ = new ExprStmt(std::unique_ptr<Expr>(invokeExpr));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

stmt_list:
    /* empty */
    {
        $$ = new StmtList(std::vector<std::unique_ptr<Stmt>>());
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
    assignable ASSIGN expr
    {
        $$ = new AssignStmt{
            std::unique_ptr<Expr>($1),
            std::unique_ptr<Expr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    ;

assignable:
    IDENT
    {
        $$ = new IdentExpr(*$1);
        delete $1;
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | array_subscript_expr
    {
        $$ = $1;
    }
    | deref_expr
    {
        $$ = $1;
    }
    | factor DOT IDENT
    {
        $$ = new MemberAccessExpr(std::unique_ptr<Expr>($1), std::make_unique<IdentExpr>(*$3));
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
        delete $3;
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

for_stmt:
    FOR LPAREN declare_stmt SEMICOLON bool_expr SEMICOLON assign_stmt RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new ForStmt{
            std::unique_ptr<Stmt>($3), std::unique_ptr<BoolExpr>($5),
            std::unique_ptr<Stmt>($7), std::unique_ptr<StmtList>($10)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | FOR LPAREN assign_stmt SEMICOLON bool_expr SEMICOLON assign_stmt RPAREN LBRACE stmt_list RBRACE
    {
        $$ = new ForStmt{
            std::unique_ptr<Stmt>($3), std::unique_ptr<BoolExpr>($5),
            std::unique_ptr<Stmt>($7), std::unique_ptr<StmtList>($10)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

/* channel recv 双返回值: let val, ok = ch.recv(); */
channel_recv_stmt:
    LET IDENT COMMA IDENT ASSIGN factor DOT IDENT LPAREN RPAREN
    {
        if (std::string(*$8) != "recv") {
            yyerror("channel 双返回值语法仅支持 recv 方法");
            YYERROR;
        }
        $$ = new ChannelRecvStmt(*$2, *$4, std::unique_ptr<Expr>($6));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
        delete $4;
        delete $8;
    }
    ;

/* range-for channel: for val in ch { ... } */
for_range_channel_stmt:
    FOR IDENT IN IDENT LBRACE stmt_list RBRACE
    {
        auto* chExpr = new IdentExpr(*$4);
        $$ = new ForRangeChannelStmt(*$2, std::unique_ptr<Expr>(chExpr),
                                     std::unique_ptr<StmtList>($6));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
        delete $4;
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

printf_stmt:
    PRINTF LPAREN arg_list RPAREN
    {
        $$ = new PrintfStmt{ std::unique_ptr<ArgList>($3) };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

scanf_stmt:
    SCANF LPAREN arg_list RPAREN
    {
        $$ = new ScanfStmt{ std::unique_ptr<ArgList>($3) };
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
    | bool_expr AND bool_expr
    {
        $$ = new BoolExpr{
            "&&", std::unique_ptr<BoolExpr>($1), std::unique_ptr<BoolExpr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | bool_expr OR bool_expr
    {
        $$ = new BoolExpr{
            "||", std::unique_ptr<BoolExpr>($1), std::unique_ptr<BoolExpr>($3)
        };
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
    }
    | NOT bool_expr
    {
        $$ = new BoolExpr{
            "!", std::unique_ptr<BoolExpr>($2)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | LPAREN bool_expr RPAREN
    {
        $$ = $2;
    }
    | TRUE_KW
    {
        $$ = new BoolExpr{
            "!=", std::make_unique<NumberExpr>(1), std::make_unique<NumberExpr>(0)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | FALSE_KW
    {
        $$ = new BoolExpr{
            "!=", std::make_unique<NumberExpr>(0), std::make_unique<NumberExpr>(0)
        };
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
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
    | term MOD factor
    {
        $$ = new BinaryExpr{
            '%', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3)
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
    | FLOATNUMBER
    {
        $$ = new FloatNumberExpr($1);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | NIL
    {
        $$ = new NilExpr();
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | TRUE_KW
    {
        $$ = new NumberExpr(1);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | FALSE_KW
    {
        $$ = new NumberExpr(0);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | STRING_LITERAL
    {
        $$ = new StringLiteralExpr(*$1);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $1;
    }
    | IDENT
    {
        $$ = new IdentExpr(*$1);
        delete $1;
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | addr_expr
    {
        $$ = $1;
    }
    | deref_expr
    {
        $$ = $1;
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
    | THIS
    {
        $$ = new IdentExpr("this", TypeInfo{ SymbolKind::Pointer, {}, 0, false });
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | NEW IDENT LPAREN RPAREN
    {
        $$ = new NewExpr(std::make_unique<IdentExpr>(*$2), nullptr);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
    }
    | NEW IDENT LPAREN arg_list RPAREN
    {
        $$ = new NewExpr(std::make_unique<IdentExpr>(*$2), std::unique_ptr<ArgList>($4));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
        delete $2;
    }
    | NEW INTSIGN LBRACKET expr RBRACKET
    {
        $$ = new NewArrayExpr("int", false, std::unique_ptr<Expr>($4));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | NEW FLOATSIGN LBRACKET expr RBRACKET
    {
        $$ = new NewArrayExpr("float", true, std::unique_ptr<Expr>($4));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | factor DOT IDENT
    {
        $$ = new MemberAccessExpr(std::unique_ptr<Expr>($1), std::make_unique<IdentExpr>(*$3));
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
        delete $3;
    }
    | factor DOT IDENT LPAREN RPAREN
    {
        $$ = new MethodCallExpr(std::unique_ptr<Expr>($1), std::make_unique<IdentExpr>(*$3), nullptr);
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
        delete $3;
    }
    | factor DOT IDENT LPAREN arg_list RPAREN
    {
        $$ = new MethodCallExpr(std::unique_ptr<Expr>($1), std::make_unique<IdentExpr>(*$3), std::unique_ptr<ArgList>($5));
        $$->lineno = @2.first_line;
        $$->column = @2.first_column;
        delete $3;
    }
    | STRLEN LPAREN expr RPAREN
    {
        $$ = new StrlenExpr(std::unique_ptr<Expr>($3));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | TYPENAME_KW LPAREN expr RPAREN
    {
        $$ = new TypenameExpr(std::unique_ptr<Expr>($3));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | FIELDCOUNT LPAREN expr RPAREN
    {
        $$ = new FieldCountExpr(std::unique_ptr<Expr>($3));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | METHODCOUNT LPAREN expr RPAREN
    {
        $$ = new MethodCountExpr(std::unique_ptr<Expr>($3));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | FIELDNAME LPAREN expr COMMA expr RPAREN
    {
        $$ = new FieldNameExpr(std::unique_ptr<Expr>($3), std::unique_ptr<Expr>($5));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | METHODNAME LPAREN expr COMMA expr RPAREN
    {
        $$ = new MethodNameExpr(std::unique_ptr<Expr>($3), std::unique_ptr<Expr>($5));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | INVOKE LPAREN expr COMMA expr RPAREN
    {
        $$ = new InvokeExpr(std::unique_ptr<Expr>($3), std::unique_ptr<Expr>($5), nullptr);
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    | INVOKE LPAREN expr COMMA expr COMMA arg_list RPAREN
    {
        $$ = new InvokeExpr(std::unique_ptr<Expr>($3), std::unique_ptr<Expr>($5), std::unique_ptr<ArgList>($7));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

addr_expr:
    ANDSIGN factor %prec DEREF
    {
        $$ = new AddressOfExpr(std::unique_ptr<Expr>($2));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
    }
    ;

deref_expr:
    STAR factor %prec DEREF
    {
        $$ = new DereferenceExpr(std::unique_ptr<Expr>($2));
        $$->lineno = @1.first_line;
        $$->column = @1.first_column;
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
    base_type
    {
        $$ = $1;
    }
    |
    LBRACKET dim_list RBRACKET
    {
        $$ = new TypeInfo{ SymbolKind::Array, *$2, 0, false };
        delete $2;
    }
    |
    LBRACKET dim_list RBRACKET base_type
    {
        $$ = new TypeInfo{ SymbolKind::Array, *$2, 0, $4->isFloat };
        delete $2;
        delete $4;
    }
    | STAR type_info %prec DEREF
    {
        $$ = new TypeInfo(*$2);
        $$->pointerLevel += 1;
        if ($$->kind == SymbolKind::Int || $$->kind == SymbolKind::Pointer || $$->kind == SymbolKind::Float) {
            $$->kind = SymbolKind::Pointer;
        }
    }
    ;

base_type:
    INTSIGN
    {
        $$ = new TypeInfo{ SymbolKind::Int, {}, 0, false };
    }
    | FLOATSIGN
    {
        $$ = new TypeInfo{ SymbolKind::Float, {}, 0, true };
    }
    | STRINGSIGN
    {
        $$ = new TypeInfo{ SymbolKind::String, {}, 0, false };
    }
    | IDENT
    {
        $$ = new TypeInfo{ SymbolKind::Class, {}, 0, false, *$1 };
        delete $1;
    }
    | VECTOR LT type_info GT
    {
        $$ = new TypeInfo{ SymbolKind::Vector, {}, 0, false };
        $$->typeParams.push_back(*$3);
        delete $3;
    }
    | MAP LT type_info COMMA type_info GT
    {
        $$ = new TypeInfo{ SymbolKind::Map, {}, 0, false };
        $$->typeParams.push_back(*$3);
        $$->typeParams.push_back(*$5);
        delete $3;
        delete $5;
    }
    | DEQUE LT type_info GT
    {
        $$ = new TypeInfo{ SymbolKind::Deque, {}, 0, false };
        $$->typeParams.push_back(*$3);
        delete $3;
    }
    | QUEUE LT type_info GT
    {
        $$ = new TypeInfo{ SymbolKind::Queue, {}, 0, false };
        $$->typeParams.push_back(*$3);
        delete $3;
    }
    | CHANNEL LT type_info GT
    {
        $$ = new TypeInfo{ SymbolKind::Channel, {}, 0, false };
        $$->typeParams.push_back(*$3);
        delete $3;
    }
    | CHANNEL LT type_info COMMA NUMBER GT
    {
        $$ = new TypeInfo{ SymbolKind::Channel, {}, 0, false };
        $$->typeParams.push_back(*$3);
        $$->channelCapacity = $5;
        delete $3;
    }
    ;
%%