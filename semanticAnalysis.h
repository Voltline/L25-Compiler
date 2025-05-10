#pragma once
#include <string>
#include <vector>
#include "symbol.h"
#include "ast.h"

class SemanticAnalyzer
{
public:
    void analyze(const Program& program);

private:
    SymbolTableManager symbolManager;

    void analyzeProgram(const Program& program);
    void analyzeFunc(const Func& func);
    void analyzeStmt(const Stmt& stmt);
    void analyzeExpr(const Expr& expr);

    // 第三个参数用于描述数组维度
    void declareSymbol(const std::string& name, const SymbolInfo& info);

    bool checkSameScopeSymbolExists(const std::string& name);
    bool checkSymbolExists(const std::string& name);
};