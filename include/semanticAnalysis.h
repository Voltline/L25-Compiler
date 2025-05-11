#pragma once
#include <string>
#include <vector>
#include "symbol.h"
#include "ast.h"

class SemanticAnalyzer
{
public:
    void analyze(Program& program);
    std::unique_ptr<Scope> rootScope = nullptr;
private:
    Scope* currentScope = nullptr;

    void analyzeProgram(Program& program);
    void analyzeFunc(Func& func);
    void analyzeStmt(Stmt& stmt);
    void analyzeExpr(Expr& expr);

    void declareSymbol(const std::string& name, const SymbolInfo& info);

    bool checkSameScopeSymbolExists(const std::string& name);
    bool checkSymbolExists(const std::string& name);

    bool checkSymbolTypeMatch(const std::string& name, const TypeInfo& type);

    Scope* enterScope();
    void exitScope();
};