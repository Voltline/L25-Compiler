#include "semanticAnalysis.h"

/* SemanticAnalyzer 方法定义 */
// Public
void SemanticAnalyzer::analyze(const Program& program)
{
    symbolManager.enterScope();
    analyzeProgram(program);
    symbolManager.exitScope();
}

// Private
void SemanticAnalyzer::analyzeProgram(const Program& program)
{
    SymbolInfo progInfo{ SymbolKind::Program, program.name->ident };
    declareSymbol(program.name->ident, progInfo);
    // 先做符号表生成
    for (const auto& func: program.functions) {
        SymbolInfo funcInfo{ SymbolKind::Function, program.name->ident };
        // TODO: 函数参数类型列表还没做
        declareSymbol(func->name->ident, funcInfo);
    }

    // 再对函数内符号表单独语义分析
    for (const auto& func: program.functions) {
        analyzeFunc(*func);
    }

    // 再对每个Stmt单独做语义分析
    for (const auto& stmt: program.main_body->stmts) {
        analyzeStmt(*stmt);
    }
}

void SemanticAnalyzer::analyzeFunc(const Func& func)
{
    symbolManager.enterScope();    
    if (!func.params->params.empty()) {
        for (const auto& param: func.params->params) {
            // TODO: 这里可能有求值存入value的需求
            SymbolInfo paramInfo{ SymbolKind::Int, param->ident };
            declareSymbol(param->ident, paramInfo);
        }
    }
    for (const auto& stmt: func.stmts->stmts) {
        analyzeStmt(*stmt);
    }
    analyzeExpr(*func.return_value);
    symbolManager.exitScope();
}

void SemanticAnalyzer::analyzeStmt(const Stmt& stmt)
{
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

void SemanticAnalyzer::analyzeExpr(const Expr& expr)
{
    if (auto ident = dynamic_cast<const IdentExpr*>(&expr)) {
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
    symbolManager.declare(name, info);
}

bool SemanticAnalyzer::checkSameScopeSymbolExists(const std::string& name)
{
    return symbolManager.lookupInplace(name) != nullptr;
}

bool SemanticAnalyzer::checkSymbolExists(const std::string& name)
{
    return symbolManager.lookup(name) != nullptr;
}