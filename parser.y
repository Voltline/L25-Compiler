%{
#include "ast.h"
#include <memory>
#include <cstdio>
#include <cstdlib>
#include <string>

int yylex();
void yyerror(const char* msg) { fprintf(stderr, "Error: %s\n", msg); }
extern Expr* rootExpr;
%}

%union {
    int num;
    std::string* ident;
    Expr* expr;
}

%debug

%token <num> NUMBER
%token <ident> IDENT
%token PROGRAM FUNC MAIN LET IF ELSE WHILE INPUT OUTPUT RETURN
%token PLUS MINUS MULTIPLY DIVIDE EQ NEQ LT LE GT GE ASSIGN
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA
%left PLUS MINUS
%left MULTIPLY DIVIDE
%type <expr> expr

%%
input:
    expr {
        rootExpr = $1;
    }
;

expr:
    expr PLUS expr  { 
        $$ = new BinaryExpr{ 
            '+', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3) 
        };
    }
    | expr MINUS expr  { 
        $$ = new BinaryExpr{ 
            '-', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3) 
        };
    }
    | expr MULTIPLY expr {
        $$ = new BinaryExpr{ 
            '*', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3) 
        };
    }
    | expr DIVIDE expr {
        $$ = new BinaryExpr{ 
            '/', std::unique_ptr<Expr>($1), std::unique_ptr<Expr>($3) 
        };
    }
    | NUMBER {
        $$ = new NumberExpr($1);
    }
    | IDENT {
        $$ = new IdentExpr(*$1);
        delete $1;
    }
    ;
%%