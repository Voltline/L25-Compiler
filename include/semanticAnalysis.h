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
    std::vector<Func*> funcStack; // 用于记录当前函数链以发现捕获

    void analyzeProgram(Program& program);
    void analyzeFunc(Func& func);
    void analyzeStmt(Stmt& stmt);
    void analyzeExpr(Expr& expr);

    void declareSymbol(const std::string& name, const SymbolInfo& info);

    bool checkSameScopeSymbolExists(const std::string& name);
    bool checkSymbolExists(const std::string& name);

    bool checkSymbolTypeMatch(const std::string& name, const TypeInfo& type);

    Scope* findSymbolScope(const std::string& name);

    Scope* enterScope();
    void exitScope();
    void reportError(const ASTNode& node, const std::string& msg);
};