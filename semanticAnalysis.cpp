#include "semanticAnalysis.h"

/* SemanticAnalyzer 方法定义 */
// Public
void SemanticAnalyzer::analyze(Program& program)
{
    rootScope = std::make_unique<Scope>(nullptr);
    
    currentScope = rootScope.get();
    analyzeProgram(program);
    currentScope = nullptr;
}

// Private
void SemanticAnalyzer::analyzeProgram(Program& program)
{
    program.scope = currentScope;
    // 声明程序名为一个特殊符号
    SymbolInfo progInfo{ SymbolKind::Program, program.name->ident };
    declareSymbol(program.name->ident, progInfo);

    // 存储合法函数以便后续语义分析
    std::vector<Func*> validFuncs;

    // 第一步：构建函数的符号表
    for (auto& func : program.functions) {
        const std::string& funcName = func->name->ident;

        if (checkSameScopeSymbolExists(funcName)) {
            std::cerr << "错误: 函数 " << funcName << " 重定义" << std::endl;
            continue;
        }

        SymbolInfo funcInfo{ funcName, *func };
        declareSymbol(funcName, funcInfo);

        validFuncs.push_back(func.get()); // 只记录合法函数
    }

    // 第二步：分析合法函数体
    for (auto* func : validFuncs) {
        analyzeFunc(*func);
    }

    // 第三步：分析 main 函数体
    enterScope();
    for (auto& stmt : program.main_body->stmts) {
        analyzeStmt(*stmt);
    }
    exitScope();
}

void SemanticAnalyzer::analyzeFunc(Func& func)
{
    func.scope = currentScope;
    enterScope();    
    if (func.params) {
        for (const auto& param: func.params->params) {
            // TODO: 这里可能有求值存入value的需求
            SymbolInfo paramInfo{ param->ident, param->type };
            declareSymbol(param->ident, paramInfo);
        }
    }
    for (const auto& stmt: func.stmts->stmts) {
        analyzeStmt(*stmt);
    }
    analyzeExpr(*func.return_value);
    exitScope();
}

void SemanticAnalyzer::analyzeStmt(Stmt& stmt)
{
    stmt.scope = currentScope;
    if (auto decl = dynamic_cast<const DeclareStmt*>(&stmt)) {
        if (checkSameScopeSymbolExists(decl->name->ident)) {
            std::cerr << decl->name->ident << " 已存在，出现重定义行为" << std::endl;
        } else {
            SymbolInfo info{ SymbolKind::Int, decl->name->ident };
            declareSymbol(decl->name->ident, info);
        }
        if (decl->expr) {
            analyzeExpr(*decl->expr);
        }
    } else if (auto assign = dynamic_cast<const AssignStmt*>(&stmt)) {
        if (!checkSymbolExists(assign->name->ident)) {
            std::cerr << assign->name->ident << " 未声明" << std::endl;
        }
        analyzeExpr(*assign->expr);
    } else if (auto ifStmt = dynamic_cast<const IfStmt*>(&stmt)) {
        const auto& boolExpr = *ifStmt->condition;
        analyzeExpr(*boolExpr.lhs);
        analyzeExpr(*boolExpr.rhs);

        for (const auto& stmt: ifStmt->if_body->stmts) {
            analyzeStmt(*stmt);
        }

        if (ifStmt->else_body) {
            for (const auto& stmt: ifStmt->else_body->stmts) {
                analyzeStmt(*stmt);
            }
        }
    } else if (auto whileStmt = dynamic_cast<const WhileStmt*>(&stmt)) {
        const auto& boolExpr = *whileStmt->condition;
        analyzeExpr(*boolExpr.lhs);
        analyzeExpr(*boolExpr.rhs);

        for (const auto& stmt: whileStmt->loop_body->stmts) {
            analyzeStmt(*stmt);
        }
    } else if (auto funcCallStmt = dynamic_cast<const FuncCallStmt*>(&stmt)) {
        if (!checkSymbolExists(funcCallStmt->name->ident)) {
            std::cerr << funcCallStmt->name->ident << " 函数未定义" << std::endl;
            return;
        }
        if (funcCallStmt->args) {
            for (const auto& expr: funcCallStmt->args->args) {
                analyzeExpr(*expr);
            }
        }
    } else if (auto inputStmt = dynamic_cast<const InputStmt*>(&stmt)) {
        for (const auto& ident: inputStmt->idents) {
            if (!checkSymbolExists(ident->ident)) {
                std::cerr << ident->ident << " 未声明" << std::endl;
            }
        }
    } else if (auto outputStmt = dynamic_cast<const OutputStmt*>(&stmt)) {
        for (const auto& expr: outputStmt->idents) {
            analyzeExpr(*expr);
        }
    }
}

void SemanticAnalyzer::analyzeExpr(Expr& expr)
{
    expr.scope = currentScope;
    if (auto ident = dynamic_cast<const IdentExpr*>(&expr)) {
        if (ident->ident.empty()) {
            std::cerr << "警告: IdentExpr 中的 ident 字符串为空！" << std::endl;
        }

        if (!checkSameScopeSymbolExists(ident->ident)) {
            std::cerr << ident->ident << " 不存在" << std::endl;
        }
    } else if (auto binary = dynamic_cast<const BinaryExpr*>(&expr)) {
        analyzeExpr(*binary->lhs);
        analyzeExpr(*binary->rhs);
    } else if (auto unary = dynamic_cast<const UnaryExpr*>(&expr)) {
        analyzeExpr(*unary->rhs);
    } else if (auto funcCallExpr = dynamic_cast<const FuncCallExpr*>(&expr)) {
        if (!checkSymbolExists(funcCallExpr->name->ident)) {
            std::cerr << funcCallExpr->name->ident << " 函数未定义" << std::endl;
            return;
        }
        if (funcCallExpr->args) {
            for (const auto& expr: funcCallExpr->args->args) {
                analyzeExpr(*expr);
            }
        }
    }
}

void SemanticAnalyzer::declareSymbol(const std::string& name, const SymbolInfo& info)
{
    if (!currentScope) {
        std::cerr << "错误: 作用域未初始化，无法声明符号" << name << std::endl;
        return;
    }
    currentScope->declare(name, info);
}

bool SemanticAnalyzer::checkSameScopeSymbolExists(const std::string& name)
{
    return currentScope->lookupLocal(name) != nullptr;
}

bool SemanticAnalyzer::checkSymbolExists(const std::string& name)
{
    return currentScope->lookup(name) != nullptr;
}

Scope* SemanticAnalyzer::enterScope()
{
    currentScope = currentScope->createChild();
    return currentScope;
}

void SemanticAnalyzer::exitScope()
{
    if (!currentScope) {
        std::cerr << "错误: 已位于全局作用域, 无法退出" << std::endl;
        return;
    }
    currentScope = currentScope->getParent();
}