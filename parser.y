%{
#include "ast.h"
#include <memory>
#include <cstdio>
#include <cstdlib>

int yylex();
void yyerror(const char* msg) { fprintf(stderr, "Error: %s\n", msg); }
extern Expr* rootExpr;
%}

%union {
    int num;
    Expr* expr;
}

%token <num> NUMBER
%token PLUS MINUS
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
    ;
%%