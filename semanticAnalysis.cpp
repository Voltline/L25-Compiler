#include "include/semanticAnalysis.h"
#include "include/errorReporter.h"
#include <algorithm>
extern bool hasError;

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

    // 第一步：构建函数的符号表
    for (auto& func : program.functions) {
        const std::string& funcName = func->name->ident;

        if (checkSameScopeSymbolExists(funcName)) {
            reportError(*func, "函数 " + funcName + " 重定义");
            continue;
        }
        
        SymbolInfo funcInfo{ funcName, *func };
        declareSymbol(funcName, funcInfo);

        analyzeFunc(*func);
    }

    // 第二步：分析 main 函数体
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
    func.body_scope = currentScope;
    funcStack.push_back(&func);
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
    funcStack.pop_back();
    exitScope();
}

void SemanticAnalyzer::analyzeStmt(Stmt& stmt)
{
    stmt.scope = currentScope;
    if (auto decl = dynamic_cast<const DeclareStmt*>(&stmt)) {
        if (checkSameScopeSymbolExists(decl->name->ident)) {
            reportError(*decl, "变量重定义：" + decl->name->ident);
        } else {
            SymbolInfo info{ decl->name->ident, decl->name->type };
            declareSymbol(decl->name->ident, info);
        }
        if (decl->expr) {
            analyzeExpr(*decl->expr);
        }
    } else if (auto assign = dynamic_cast<const AssignStmt*>(&stmt)) {
        if (auto normalVarAssign = dynamic_cast<const IdentExpr*>(assign->name.get())) {
            if (!checkSymbolExists(normalVarAssign->ident)) {
                reportError(*normalVarAssign, "变量未声明：" + normalVarAssign->ident);
                return;
            }
            analyzeExpr(*assign->expr);
        } else if (auto arrayAssign = dynamic_cast<ArraySubscriptExpr*>(assign->name.get())) {
            if (!checkSymbolExists(arrayAssign->array->ident)) {
                reportError(*arrayAssign, "变量未声明：" + arrayAssign->array->ident);
                return;
            }
            analyzeExpr(*arrayAssign);
            analyzeExpr(*assign->expr);
        } else if (auto derefAssign = dynamic_cast<DereferenceExpr*>(assign->name.get())) {
            analyzeExpr(*derefAssign);
            analyzeExpr(*assign->expr);
        }
    } else if (auto ifStmt = dynamic_cast<IfStmt*>(&stmt)) {
        const auto& boolExpr = *ifStmt->condition;
        analyzeExpr(*boolExpr.lhs);
        analyzeExpr(*boolExpr.rhs);

        enterScope();
        ifStmt->ifScope = currentScope;
        for (const auto& stmt: ifStmt->if_body->stmts) {
            analyzeStmt(*stmt);
        }
        exitScope();

        if (ifStmt->else_body) {
            enterScope();
            ifStmt->elseScope = currentScope;

            for (const auto& stmt: ifStmt->else_body->stmts) {
                analyzeStmt(*stmt);
            }
            exitScope();
        }
    } else if (auto whileStmt = dynamic_cast<WhileStmt*>(&stmt)) {
        const auto& boolExpr = *whileStmt->condition;
        analyzeExpr(*boolExpr.lhs);
        analyzeExpr(*boolExpr.rhs);

        enterScope();
        whileStmt->loopBodyScope = currentScope;
        for (const auto& stmt: whileStmt->loop_body->stmts) {
            analyzeStmt(*stmt);
        }
        exitScope();
    } else if (auto funcCallStmt = dynamic_cast<const FuncCallStmt*>(&stmt)) {
        if (!checkSymbolExists(funcCallStmt->name->ident)) {
            reportError(*funcCallStmt, "函数未声明：" + funcCallStmt->name->ident);
            return;
        }
        if (funcCallStmt->args) {
            for (const auto& expr: funcCallStmt->args->args) {
                analyzeExpr(*expr);
            }
        }
    } else if (auto inputStmt = dynamic_cast<const InputStmt*>(&stmt)) {
        for (auto& ident: inputStmt->idents) {
            if (const auto* idExpr = dynamic_cast<IdentExpr*>(ident.get())) {
                if (!checkSymbolExists(idExpr->ident)) {
                    reportError(*idExpr, "变量未声明：" + idExpr->ident);
                }
            } else if (auto* arraySubscriptExpr = dynamic_cast<ArraySubscriptExpr*>(ident.get())) {
                analyzeExpr(*arraySubscriptExpr);
            } else {
                reportError(*ident, "不允许出现在输入列表的类型");
            }

        }
    } else if (auto outputStmt = dynamic_cast<const OutputStmt*>(&stmt)) {
        for (const auto& expr: outputStmt->idents) {
            analyzeExpr(*expr);
        }
    } else if (auto funcDefStmt = dynamic_cast<Func*>(&stmt)) {
        const std::string& funcName = funcDefStmt->name->ident;

        if (checkSameScopeSymbolExists(funcName)) {
            reportError(*funcDefStmt, "函数 " + funcName + " 重定义");
            return;
        }

        SymbolInfo funcInfo{ funcName, *funcDefStmt };
        declareSymbol(funcName, funcInfo);
        analyzeFunc(*funcDefStmt);
    }
}

void SemanticAnalyzer::analyzeExpr(Expr& expr)
{
    expr.scope = currentScope;
    if (auto ident = dynamic_cast<const IdentExpr*>(&expr)) {
        // 最小操作单位ident，绑定或不绑定scope没有区别
        if (ident->ident.empty()) {
            reportError(*ident, "变量名为空");
        }

        if (!checkSymbolExists(ident->ident)) {
            reportError(*ident, "变量未声明：" + ident->ident);
        }

        // 记录闭包捕获
        if (!funcStack.empty()) {
            Scope* declScope = findSymbolScope(ident->ident);
            if (declScope && declScope != funcStack.back()->body_scope) {
                SymbolInfo* capturedSymbol = currentScope->lookup(ident->ident);
                if (capturedSymbol && capturedSymbol->kind != SymbolKind::Function && capturedSymbol->kind != SymbolKind::Program) {
                    auto& captureList = funcStack.back()->captures;
                    if (std::find(captureList.begin(), captureList.end(), capturedSymbol) == captureList.end()) {
                        captureList.push_back(capturedSymbol);
                    }
                }
            }
        }
    } else if (auto binary = dynamic_cast<const BinaryExpr*>(&expr)) {
        analyzeExpr(*binary->lhs);
        analyzeExpr(*binary->rhs);
    } else if (auto unary = dynamic_cast<const UnaryExpr*>(&expr)) {
        analyzeExpr(*unary->rhs);
    } else if (auto funcCallExpr = dynamic_cast<const FuncCallExpr*>(&expr)) {
        if (!checkSymbolExists(funcCallExpr->name->ident)) {
            reportError(*funcCallExpr, "函数未声明：" + funcCallExpr->name->ident);
            return;
        }
        // 函数符号
        SymbolInfo* funcSymbol{ currentScope->lookup(funcCallExpr->name->ident) };
        if (!funcCallExpr->args) {
            if (!funcSymbol->paramTypes.empty()) {
                reportError(*funcCallExpr, "函数调用参数数量不匹配：" + funcCallExpr->name->ident + " 调用需要" + std::to_string(funcSymbol->paramTypes.size()) + "个参数，但调用时未传入参数" );
                return;
            }
        }

        if (funcCallExpr->args) {
            if (funcCallExpr->args->args.size() != funcSymbol->paramTypes.size()) {
                reportError(*funcCallExpr, "函数调用参数数量不匹配：" + funcCallExpr->name->ident + " 调用需要" + std::to_string(funcSymbol->paramTypes.size()) + "个参数，但调用时传入" + std::to_string(funcCallExpr->args->args.size()) + "个参数");
                    return;
            }
            // TODO: 加上参数对应类型检查，这里还有数组传入的问题
            for (const auto& expr: funcCallExpr->args->args) {
                analyzeExpr(*expr);
            }
        }
    } else if (auto subscript = dynamic_cast<const ArraySubscriptExpr*>(&expr)) {
        if (!subscript->array) {
            reportError(expr, "数组访问表达式非法，缺失数组对象");
            return;
        }
        analyzeExpr(*subscript->array);
        // 数组符号
        SymbolInfo* arraySymbol{ currentScope->lookup(subscript->array->ident) };
        if (!arraySymbol) {
            reportError(expr, "数组未声明：" + subscript->array->ident);
            return;
        }
        if (arraySymbol->kind != SymbolKind::Array) {
            reportError(*subscript, "尝试访问非数组变量的下标：" + arraySymbol->name);
            return;
        }
        for (auto& idxExpr: subscript->subscript) {
            analyzeExpr(*idxExpr);
        }
        if (arraySymbol->dimensions.size() != subscript->subscript.size()) {
            reportError(*subscript, "下标访问与数组维度不匹配：" + subscript->array->ident);
            return;
        }
    } else if (auto addrExpr = dynamic_cast<const AddressOfExpr*>(&expr)) {
        analyzeExpr(*addrExpr->target);
    } else if (auto derefExpr = dynamic_cast<const DereferenceExpr*>(&expr)) {
        analyzeExpr(*derefExpr->pointerExpr);
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

bool SemanticAnalyzer::checkSymbolTypeMatch(const std::string& name, const TypeInfo& type)
{
    if (!checkSymbolExists(name)) {
        return false;
    }
    SymbolInfo* symbolInTable{ currentScope->lookup(name) };
    if (symbolInTable->kind == type.kind && symbolInTable->isFloat == type.isFloat) {
        if (type.kind != SymbolKind::Array) {
            return true;
        }
        return (type.dims == symbolInTable->dimensions);
    }
    return false;
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

Scope* SemanticAnalyzer::findSymbolScope(const std::string& name)
{
    Scope* scopeIter = currentScope;
    while (scopeIter) {
        if (scopeIter->lookupLocal(name)) {
            return scopeIter;
        }
        scopeIter = scopeIter->getParent();
    }
    return nullptr;
}

void SemanticAnalyzer::reportError(const ASTNode& node, const std::string& msg)
{
    reportErrorAt(node, "语义分析", msg);
}