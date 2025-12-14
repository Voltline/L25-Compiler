#pragma once
#include <string>
#include <vector>
#include <unordered_map>
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
    std::unordered_map<std::string, ClassDecl*> classDecls; // 已注册的类声明
    ClassDecl* currentClass = nullptr;

    void analyzeClass(ClassDecl& cls);
    void analyzeMethod(MethodDecl& method);
    void analyzeCtor(CtorDecl& ctor);

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