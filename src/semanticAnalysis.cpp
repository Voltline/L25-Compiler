#include "semanticAnalysis.h"
#include "errorReporter.h"
#include <algorithm>
extern bool hasError;

namespace {

bool isPointerLike(const TypeInfo& type)
{
    return type.pointerLevel > 0 || type.kind == SymbolKind::Pointer || (type.kind == SymbolKind::Class && type.pointerLevel > 0);
}

bool isZeroLiteral(const Expr* expr)
{
    if (auto num = dynamic_cast<const NumberExpr*>(expr)) {
        return num->value == 0;
    }
    return false;
}

void warnZeroAsNil(const ASTNode& node, const TypeInfo& targetType, const Expr* expr)
{
    if (expr && isPointerLike(targetType) && isZeroLiteral(expr)) {
        reportWarningAt(node, "语义分析", "建议使用 nil 表示空指针，而非 0");
    }
}

} // namespace

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

    // ===== 注册内置函数 =====
    auto registerBuiltin = [&](const std::string& name, const std::string& llvmName,
                               const std::vector<TypeInfo>& params, TypeInfo retType) {
        SymbolInfo info(SymbolKind::Function, name);
        info.kind = SymbolKind::Function;
        info.llvmName = llvmName;
        info.paramTypes = params;
        info.returnType = retType;
        info.isBuiltin = true;
        declareSymbol(name, info);
    };

    TypeInfo voidType(SymbolKind::Invalid, {});
    TypeInfo intType(SymbolKind::Int, {});
    TypeInfo int64Type(SymbolKind::Int, {}); // int 即 i32，但 GC 返回 i64 → codegen 做 trunc
    TypeInfo floatType(SymbolKind::Float, {});

    // ===== import std: 标准库函数 =====
    bool hasImportStd = false;
    for (const auto& imp : program.imports) {
        if (imp == "std") {
            hasImportStd = true;
        } else {
            // 目前仅支持 std
            // 可在此扩展其他模块
        }
    }

    if (hasImportStd) {
        registerBuiltin("std.clock_ms", "l25_clock_ms", {}, floatType);
        registerBuiltin("std.sleep_ms", "l25_sleep_ms", {intType}, voidType);
        registerBuiltin("std.exit",     "l25_exit",     {intType}, voidType);
        registerBuiltin("std.rand",     "l25_rand",     {},        intType);
        registerBuiltin("std.srand",    "l25_srand",    {intType}, voidType);
    }

    // ===== import net: 网络库函数 =====
    TypeInfo stringType(SymbolKind::String, {});
    bool hasImportNet = false;
    for (const auto& imp : program.imports) {
        if (imp == "net") hasImportNet = true;
    }
    if (hasImportNet) {
        registerBuiltin("net.tcp_listen",  "l25_net_tcp_listen",  {intType},               intType);
        registerBuiltin("net.tcp_accept",  "l25_net_tcp_accept",  {intType},               intType);
        registerBuiltin("net.tcp_connect", "l25_net_tcp_connect", {stringType, intType},    intType);
        registerBuiltin("net.tcp_send",    "l25_net_tcp_send",    {intType, stringType},    intType);
        registerBuiltin("net.tcp_recv",    "l25_net_tcp_recv",    {intType, intType},       stringType);
        registerBuiltin("net.close",       "l25_net_close",       {intType},               voidType);
        registerBuiltin("net.udp_socket",  "l25_net_udp_socket",  {},                      intType);
        registerBuiltin("net.udp_bind",    "l25_net_udp_bind",    {intType, intType},       intType);
        registerBuiltin("net.udp_sendto",  "l25_net_udp_sendto",  {intType, stringType, intType, stringType}, intType);
        registerBuiltin("net.udp_recvfrom","l25_net_udp_recvfrom",{intType, intType},       stringType);
    }

    // GC 监测函数
    registerBuiltin("gc_stats",     "l25_gc_stats",     {}, voidType);
    registerBuiltin("gc_count",     "l25_gc_count",     {}, intType);
    registerBuiltin("gc_bytes",     "l25_gc_bytes",     {}, intType);
    registerBuiltin("gc_threshold", "l25_gc_threshold", {}, intType);
    registerBuiltin("gc_set_threshold", "l25_gc_set_threshold", {intType}, voidType);
    registerBuiltin("gc_total_allocs",     "l25_gc_total_allocs",     {}, intType);
    registerBuiltin("gc_total_collections","l25_gc_total_collections",{}, intType);
    registerBuiltin("gc_total_freed",      "l25_gc_total_freed",      {}, intType);
    registerBuiltin("gc_collect",   "l25_gc_collect",   {}, voidType);
    registerBuiltin("gc_pause",     "l25_gc_pause",     {}, voidType);
    registerBuiltin("gc_resume",    "l25_gc_resume",    {}, voidType);

    // ===== 注册枚举定义 =====
    for (auto& enumDecl : program.enums) {
        enumDecl->scope = currentScope;
        for (size_t i = 0; i < enumDecl->values.size(); i++) {
            const std::string& valName = enumDecl->values[i];
            if (checkSameScopeSymbolExists(valName)) {
                reportError(*enumDecl, "枚举值重定义：" + valName);
                continue;
            }
            SymbolInfo info(SymbolKind::Int, valName);
            info.isConst = true;
            info.constIntValue = static_cast<int>(i);
            declareSymbol(valName, info);
        }
    }

    // 先注册类符号
    for (auto& cls : program.classes) {
        const std::string& className = cls->name->ident;
        if (checkSameScopeSymbolExists(className)) {
            reportError(*cls, "类重定义：" + className);
            continue;
        }
        SymbolInfo classInfo{ SymbolKind::Class, className };

        // ===== 继承处理 =====
        std::string baseName;
        if (cls->baseClass) {
            baseName = cls->baseClass->ident;
            SymbolInfo* baseSym = currentScope->lookup(baseName);
            if (!baseSym || baseSym->kind != SymbolKind::Class) {
                reportError(*cls, "基类不存在或不是类类型：" + baseName);
                continue;
            }
            // 检查循环继承
            std::string cur = baseName;
            bool circular = false;
            while (!cur.empty()) {
                if (cur == className) {
                    reportError(*cls, "循环继承：" + className);
                    circular = true;
                    break;
                }
                auto it = classBaseClass.find(cur);
                cur = (it != classBaseClass.end()) ? it->second : "";
            }
            if (circular) continue;

            // 继承基类字段
            classInfo.classFields = baseSym->classFields;
            // 继承基类方法（后面会被子类覆盖）
            classInfo.methodParamTypes = baseSym->methodParamTypes;
            classInfo.methodReturnTypes = baseSym->methodReturnTypes;
            // 继承析构函数标记
            if (baseSym->hasDestructor) classInfo.hasDestructor = true;
            // 记录继承关系
            classBaseClass[className] = baseName;
        }

        // 添加子类自身的字段（检查与基类字段重名）
        for (const auto& field : cls->fields) {
            bool duplicate = false;
            for (const auto& [fname, ftype] : classInfo.classFields) {
                if (fname == field->name->ident) {
                    reportError(*field, "字段与基类字段重名：" + field->name->ident);
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate) {
                classInfo.classFields.emplace_back(field->name->ident, field->type);
            }
        }

        classInfo.hasDestructor = classInfo.hasDestructor || static_cast<bool>(cls->dtor);

        // 添加/覆盖子类自身的方法
        for (const auto& method : cls->methods) {
            std::vector<TypeInfo> params;
            if (method->params) {
                for (const auto& param : method->params->params) {
                    params.push_back(param->type);
                }
            }
            classInfo.methodParamTypes[method->name->ident] = params;
            classInfo.methodReturnTypes[method->name->ident] = method->returnType;
        }
        declareSymbol(className, classInfo);
        classFieldLayouts[className] = classInfo.classFields;
        classMethodReturnTypes[className] = classInfo.methodReturnTypes;

        // 按定义顺序记录方法名（继承 + 新增）
        std::vector<std::string> methodNames;
        if (cls->baseClass && !baseName.empty()) {
            auto baseNamesIt = classMethodNames.find(baseName);
            if (baseNamesIt != classMethodNames.end()) {
                methodNames = baseNamesIt->second;
            }
        }
        for (const auto& method : cls->methods) {
            if (std::find(methodNames.begin(), methodNames.end(), method->name->ident) == methodNames.end()) {
                methodNames.push_back(method->name->ident);
            }
        }
        classMethodNames[className] = methodNames;
        classDecls[className] = cls.get();
    }

    for (auto& cls : program.classes) {
        analyzeClass(*cls);
    }

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
    funcDepth++;
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
    if (func.return_value) {
        analyzeExpr(*func.return_value);
        warnZeroAsNil(func, func.returnType, func.return_value.get());
    }
    funcStack.pop_back();
    funcDepth--;
    exitScope();
}

void SemanticAnalyzer::analyzeClass(ClassDecl& cls)
{
    cls.scope = currentScope;
    currentClass = &cls;
    enterScope();

    // 注册继承的基类字段
    if (cls.baseClass) {
        SymbolInfo* baseSym = rootScope ? rootScope->lookup(cls.baseClass->ident) : nullptr;
        if (baseSym && baseSym->kind == SymbolKind::Class) {
            for (const auto& [fname, ftype] : baseSym->classFields) {
                if (!checkSameScopeSymbolExists(fname)) {
                    SymbolInfo info{ fname, ftype };
                    declareSymbol(fname, info);
                }
            }
        }
    }

    // 注册字段
    for (const auto& field : cls.fields) {
        if (checkSameScopeSymbolExists(field->name->ident)) {
            reportError(*field, "字段重定义：" + field->name->ident);
            continue;
        }
        SymbolInfo info{ field->name->ident, field->type };
        declareSymbol(field->name->ident, info);
    }

    // 构造函数
    for (auto& ctor : cls.ctors) {
        analyzeCtor(*ctor);
    }

    if (cls.dtor) {
        if (cls.dtor->name->ident != cls.name->ident) {
            reportError(*cls.dtor, "析构函数名称必须与类名一致");
        }
        analyzeDtor(*cls.dtor);
    }

    // 方法
    for (auto& method : cls.methods) {
        analyzeMethod(*method);
    }
    exitScope();
    currentClass = nullptr;
}

void SemanticAnalyzer::analyzeCtor(CtorDecl& ctor)
{
    ctor.scope = currentScope;
    enterScope();
    ctor.bodyScope = currentScope;
    if (currentClass) {
        TypeInfo thisType{ SymbolKind::Class, {}, 1, false, currentClass->name->ident };
        SymbolInfo thisInfo{ "this", thisType };
        declareSymbol("this", thisInfo);
    }
    if (ctor.params) {
        for (const auto& param : ctor.params->params) {
            SymbolInfo paramInfo{ param->ident, param->type };
            declareSymbol(param->ident, paramInfo);
        }
    }
    if (ctor.body) {
        for (const auto& stmt : ctor.body->stmts) {
            analyzeStmt(*stmt);
        }
    }
    exitScope();
}

void SemanticAnalyzer::analyzeDtor(DtorDecl& dtor)
{
    dtor.scope = currentScope;
    enterScope();
    dtor.bodyScope = currentScope;
    if (currentClass) {
        TypeInfo thisType{ SymbolKind::Class, {}, 1, false, currentClass->name->ident };
        SymbolInfo thisInfo{ "this", thisType };
        declareSymbol("this", thisInfo);
    }

    if (dtor.body) {
        for (const auto& stmt : dtor.body->stmts) {
            analyzeStmt(*stmt);
        }
    }
    exitScope();
}

void SemanticAnalyzer::analyzeMethod(MethodDecl& method)
{
    method.scope = currentScope;
    enterScope();
    method.bodyScope = currentScope;
    funcDepth++;
    if (currentClass) {
        TypeInfo thisType{ SymbolKind::Class, {}, 1, false, currentClass->name->ident };
        SymbolInfo thisInfo{ "this", thisType };
        declareSymbol("this", thisInfo);
    }
    if (method.params) {
        for (const auto& param : method.params->params) {
            SymbolInfo paramInfo{ param->ident, param->type };
            declareSymbol(param->ident, paramInfo);
        }
    }
    if (method.body) {
        for (const auto& stmt : method.body->stmts) {
            analyzeStmt(*stmt);
        }
    }
    if (method.return_value) {
        analyzeExpr(*method.return_value);
        warnZeroAsNil(method, method.returnType, method.return_value.get());
    }
    funcDepth--;
    exitScope();
}

void SemanticAnalyzer::analyzeStmt(Stmt& stmt)
{
    stmt.scope = currentScope;
    if (auto decl = dynamic_cast<const DeclareStmt*>(&stmt)) {
        if (checkSameScopeSymbolExists(decl->name->ident)) {
            reportError(*decl, "变量重定义：" + decl->name->ident);
        } else {
            TypeInfo declType = decl->name->type;
            // 自动推导：如果类型为默认 Int 且 RHS 是字符串字面量，推导为 String；
            //           如果 RHS 是 new 表达式，推导为 *ClassName
            if (declType.kind == SymbolKind::Int && declType.pointerLevel == 0 && decl->expr) {
                TypeInfo rhsType = evaluateExprType(decl->expr.get());
                if (rhsType.kind == SymbolKind::String) {
                    declType = TypeInfo{ SymbolKind::String, {}, 0, false };
                    const_cast<IdentExpr*>(decl->name.get())->type = declType;
                } else if (rhsType.kind == SymbolKind::Class && !rhsType.className.empty()) {
                    declType = rhsType;
                    const_cast<IdentExpr*>(decl->name.get())->type = declType;
                }
            }
            SymbolInfo info{ decl->name->ident, declType };
            declareSymbol(decl->name->ident, info);
        }
        if (decl->expr) {
            analyzeExpr(*decl->expr);
            warnZeroAsNil(*decl, decl->name->type, decl->expr.get());
        }
    } else if (auto assign = dynamic_cast<const AssignStmt*>(&stmt)) {
        if (auto normalVarAssign = dynamic_cast<const IdentExpr*>(assign->name.get())) {
            if (!checkSymbolExists(normalVarAssign->ident)) {
                reportError(*normalVarAssign, "变量未声明：" + normalVarAssign->ident);
                return;
            }
            analyzeExpr(*assign->expr);
            SymbolInfo* target = currentScope->lookup(normalVarAssign->ident);
            if (target) {
                warnZeroAsNil(*assign, TypeInfo{ target->kind, target->dimensions, target->pointerLevel, target->isFloat, target->className }, assign->expr.get());
            }
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
        } else {
            analyzeExpr(*assign->name);
            analyzeExpr(*assign->expr);
        }
    } else if (auto ifStmt = dynamic_cast<IfStmt*>(&stmt)) {
        analyzeBoolExpr(*ifStmt->condition);

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
        analyzeBoolExpr(*whileStmt->condition);

        enterScope();
        whileStmt->loopBodyScope = currentScope;
        loopDepth++;
        for (const auto& stmt: whileStmt->loop_body->stmts) {
            analyzeStmt(*stmt);
        }
        loopDepth--;
        exitScope();
    } else if (auto forStmt = dynamic_cast<ForStmt*>(&stmt)) {
        enterScope();
        forStmt->loopBodyScope = currentScope;
        // 分析初始化语句（可能包含 let 声明）
        if (forStmt->init) {
            analyzeStmt(*forStmt->init);
        }
        analyzeBoolExpr(*forStmt->condition);
        // 分析步进语句
        if (forStmt->step) {
            analyzeStmt(*forStmt->step);
        }
        // 分析循环体
        loopDepth++;
        for (const auto& s : forStmt->loop_body->stmts) {
            analyzeStmt(*s);
        }
        loopDepth--;
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
    } else if (auto printfStmt = dynamic_cast<const PrintfStmt*>(&stmt)) {
        if (printfStmt->idents.empty()) {
            reportError(*printfStmt, "printf 至少需要一个格式字符串参数");
            return;
        }
        for (const auto& expr : printfStmt->idents) {
            analyzeExpr(*expr);
        }
    } else if (auto scanfStmt = dynamic_cast<const ScanfStmt*>(&stmt)) {
        if (scanfStmt->idents.empty()) {
            reportError(*scanfStmt, "scanf 至少需要一个格式字符串参数");
            return;
        }
        analyzeExpr(*scanfStmt->idents[0]); // 格式串
        for (size_t i = 1; i < scanfStmt->idents.size(); i++) {
            const auto& ident = scanfStmt->idents[i];
            if (const auto* idExpr = dynamic_cast<IdentExpr*>(ident.get())) {
                if (!checkSymbolExists(idExpr->ident)) {
                    reportError(*idExpr, "变量未声明：" + idExpr->ident);
                }
            } else if (auto* arraySubscriptExpr = dynamic_cast<ArraySubscriptExpr*>(ident.get())) {
                analyzeExpr(*arraySubscriptExpr);
            } else {
                reportError(*ident, "scanf 参数必须是变量或数组元素");
            }
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
    } else if (auto exprStmt = dynamic_cast<const ExprStmt*>(&stmt)) {
        if (exprStmt->expr) {
            analyzeExpr(*exprStmt->expr);
        }
    } else if (auto deleteStmt = dynamic_cast<DeleteStmt*>(&stmt)) {
        deleteStmt->scope = currentScope;
        if (deleteStmt->target) {
            analyzeExpr(*deleteStmt->target);
        }
    } else if (auto spawnStmt = dynamic_cast<SpawnStmt*>(&stmt)) {
        spawnStmt->scope = currentScope;
        // spawn 块内部语句使用子作用域
        Scope* childScope = currentScope->createChild();
        spawnStmt->bodyScope = childScope;
        Scope* savedScope = currentScope;
        currentScope = childScope;
        if (spawnStmt->body) {
            for (auto& s : spawnStmt->body->stmts) {
                analyzeStmt(*s);
            }
        }
        currentScope = savedScope;
    } else if (dynamic_cast<BreakStmt*>(&stmt)) {
        if (loopDepth <= 0) {
            reportError(stmt, "break 语句只能在循环内使用");
        }
    } else if (auto retStmt = dynamic_cast<ReturnStmt*>(&stmt)) {
        if (funcDepth <= 0) {
            reportError(stmt, "return 语句只能在函数内使用");
        }
        if (retStmt->value) {
            analyzeExpr(*retStmt->value);
        }
    } else if (auto chanRecv = dynamic_cast<ChannelRecvStmt*>(&stmt)) {
        // let val, ok = ch.recv();
        // 检查 channel 表达式
        analyzeExpr(*chanRecv->channel);
        // 获取 channel 表达式的类型
        if (auto chIdent = dynamic_cast<IdentExpr*>(chanRecv->channel.get())) {
            SymbolInfo* chSym = currentScope->lookup(chIdent->ident);
            if (chSym && chSym->kind == SymbolKind::Channel) {
                chanRecv->channelTypeInfo = TypeInfo{ SymbolKind::Channel, {}, 0, false };
                chanRecv->channelTypeInfo.typeParams = chSym->typeParams;
            } else {
                reportError(*chanRecv, "recv 双返回值需要 channel 类型");
            }
        }
        // 声明 val 和 ok 变量
        if (checkSameScopeSymbolExists(chanRecv->valName)) {
            reportError(*chanRecv, "变量重定义：" + chanRecv->valName);
        } else {
            TypeInfo valType = chanRecv->channelTypeInfo.typeParams.empty()
                                 ? TypeInfo{ SymbolKind::Int, {}, 0, false }
                                 : chanRecv->channelTypeInfo.typeParams[0];
            declareSymbol(chanRecv->valName, SymbolInfo{ chanRecv->valName, valType });
        }
        if (checkSameScopeSymbolExists(chanRecv->okName)) {
            reportError(*chanRecv, "变量重定义：" + chanRecv->okName);
        } else {
            TypeInfo okType{ SymbolKind::Int, {}, 0, false };
            declareSymbol(chanRecv->okName, SymbolInfo{ chanRecv->okName, okType });
        }
    } else if (auto forRange = dynamic_cast<ForRangeChannelStmt*>(&stmt)) {
        // for val in ch { ... }
        analyzeExpr(*forRange->channel);
        // 获取 channel 类型信息
        if (auto chIdent = dynamic_cast<IdentExpr*>(forRange->channel.get())) {
            SymbolInfo* chSym = currentScope->lookup(chIdent->ident);
            if (chSym && chSym->kind == SymbolKind::Channel) {
                forRange->channelTypeInfo = TypeInfo{ SymbolKind::Channel, {}, 0, false };
                forRange->channelTypeInfo.typeParams = chSym->typeParams;
            } else {
                reportError(*forRange, "for-in 需要 channel 类型");
            }
        }
        enterScope();
        forRange->loopBodyScope = currentScope;
        // 声明 val 循环变量
        TypeInfo valType = forRange->channelTypeInfo.typeParams.empty()
                             ? TypeInfo{ SymbolKind::Int, {}, 0, false }
                             : forRange->channelTypeInfo.typeParams[0];
        declareSymbol(forRange->valName, SymbolInfo{ forRange->valName, valType });
        loopDepth++;
        for (auto& s : forRange->body->stmts) {
            analyzeStmt(*s);
        }
        loopDepth--;
        exitScope();
    } else if (auto selectStmt = dynamic_cast<SelectStmt*>(&stmt)) {
        // select { case val = ch.recv(): { ... } case ch.send(x): { ... } default: { ... } }
        bool hasDefault = false;
        for (auto& c : selectStmt->cases) {
            if (c->kind == SelectCaseKind::Default) {
                if (hasDefault) {
                    reportError(*selectStmt, "select 语句只能有一个 default 分支");
                }
                hasDefault = true;
                // default 分支只有 body
                enterScope();
                c->bodyScope = currentScope;
                for (auto& s : c->body->stmts) {
                    analyzeStmt(*s);
                }
                exitScope();
            } else {
                // Recv 或 Send：分析 channel 表达式
                analyzeExpr(*c->channel);
                // 获取 channel 类型信息
                if (auto chIdent = dynamic_cast<IdentExpr*>(c->channel.get())) {
                    SymbolInfo* chSym = currentScope->lookup(chIdent->ident);
                    if (chSym && chSym->kind == SymbolKind::Channel) {
                        c->channelTypeInfo = TypeInfo{ SymbolKind::Channel, {}, 0, false };
                        c->channelTypeInfo.typeParams = chSym->typeParams;
                    } else {
                        reportError(*selectStmt, "select case 需要 channel 类型");
                    }
                }

                if (c->kind == SelectCaseKind::Send) {
                    // 分析 send 的值表达式
                    analyzeExpr(*c->sendValue);
                }

                // 进入 body 作用域
                enterScope();
                c->bodyScope = currentScope;

                if (c->kind == SelectCaseKind::Recv) {
                    // 声明 recv 变量
                    TypeInfo valType = c->channelTypeInfo.typeParams.empty()
                                         ? TypeInfo{ SymbolKind::Int, {}, 0, false }
                                         : c->channelTypeInfo.typeParams[0];
                    declareSymbol(c->recvVarName, SymbolInfo{ c->recvVarName, valType });
                }

                for (auto& s : c->body->stmts) {
                    analyzeStmt(*s);
                }
                exitScope();
            }
        }
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
            if (declScope) {
                bool insideCurrentFunc = false;
                for (Scope* walk = declScope; walk; walk = walk->getParent()) {
                    if (walk == funcStack.back()->body_scope) {
                        insideCurrentFunc = true;
                        break;
                    }
                }

                if (!insideCurrentFunc) {
                    SymbolInfo* capturedSymbol = currentScope->lookup(ident->ident);
                    if (capturedSymbol && capturedSymbol->kind != SymbolKind::Function && capturedSymbol->kind != SymbolKind::Program
                        && !capturedSymbol->isConst) {
                        auto& captureList = funcStack.back()->captures;
                        if (std::find(captureList.begin(), captureList.end(), capturedSymbol) == captureList.end()) {
                            captureList.push_back(capturedSymbol);
                        }
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
        if (!funcSymbol->isBuiltin) {
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
                for (size_t i = 0; i < funcCallExpr->args->args.size(); ++i) {
                    const auto& expr = funcCallExpr->args->args[i];
                    analyzeExpr(*expr);
                    if (i < funcSymbol->paramTypes.size()) {
                        warnZeroAsNil(*funcCallExpr, funcSymbol->paramTypes[i], expr.get());
                    }
                }
            }
        } else {
            // 内置函数：仍需分析参数表达式
            if (funcCallExpr->args) {
                for (const auto& expr : funcCallExpr->args->args) {
                    analyzeExpr(*expr);
                }
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
        // 允许 Vector、Map、Deque 的下标访问
        if (arraySymbol->kind == SymbolKind::Vector || arraySymbol->kind == SymbolKind::Map
            || arraySymbol->kind == SymbolKind::Deque) {
            if (subscript->subscript.size() != 1) {
                reportError(*subscript, "容器下标访问只允许一个索引");
                return;
            }
            for (auto& idxExpr: subscript->subscript) {
                analyzeExpr(*idxExpr);
            }
        } else if (arraySymbol->kind == SymbolKind::Array) {
            for (auto& idxExpr: subscript->subscript) {
                analyzeExpr(*idxExpr);
            }
            if (arraySymbol->dimensions.size() != subscript->subscript.size()) {
                reportError(*subscript, "下标访问与数组维度不匹配：" + subscript->array->ident);
                return;
            }
        } else if (arraySymbol->kind == SymbolKind::Pointer && arraySymbol->pointerLevel > 0) {
            // 指针下标访问（new T[n] 返回的堆指针）
            if (subscript->subscript.size() != 1) {
                reportError(*subscript, "指针下标访问只允许一个索引");
                return;
            }
            for (auto& idxExpr: subscript->subscript) {
                analyzeExpr(*idxExpr);
            }
        } else {
            reportError(*subscript, "尝试访问非数组/非容器变量的下标：" + arraySymbol->name);
            return;
        }
    } else if (auto addrExpr = dynamic_cast<const AddressOfExpr*>(&expr)) {
        analyzeExpr(*addrExpr->target);
    } else if (auto derefExpr = dynamic_cast<const DereferenceExpr*>(&expr)) {
        analyzeExpr(*derefExpr->pointerExpr);
    } else if (auto member = dynamic_cast<const MemberAccessExpr*>(&expr)) {
        analyzeExpr(*member->target);
        TypeInfo targetType = evaluateExprType(member->target.get());
        std::string className = targetType.className;
        if (targetType.kind == SymbolKind::Class && className.empty()) {
            className = member->target->scope ? member->target->scope->lookup(member->member->ident)->name : "";
        }
        if (targetType.pointerLevel > 0 && targetType.kind == SymbolKind::Class) {
            targetType.pointerLevel -= 1;
        }
        if (targetType.kind != SymbolKind::Class || className.empty()) {
            reportError(*member, "成员访问目标不是类类型");
            return;
        }
        SymbolInfo* clsInfo = rootScope ? rootScope->lookup(className) : nullptr;
        if (!clsInfo || clsInfo->kind != SymbolKind::Class) {
            reportError(*member, "未知的类：" + className);
            return;
        }
        auto it = std::find_if(clsInfo->classFields.begin(), clsInfo->classFields.end(), [&](const auto& f){return f.first == member->member->ident;});
        if (it == clsInfo->classFields.end()) {
            reportError(*member, "类中不存在成员：" + member->member->ident);
        }
    } else if (auto methodCall = dynamic_cast<const MethodCallExpr*>(&expr)) {
        // 检查是否为命名空间限定的函数调用（如 std.rand()）
        if (auto* identTarget = dynamic_cast<IdentExpr*>(methodCall->target.get())) {
            std::string qualifiedName = identTarget->ident + "." + methodCall->method->ident;
            SymbolInfo* funcSym = currentScope->lookup(qualifiedName);
            if (funcSym && funcSym->kind == SymbolKind::Function) {
                // 标记为命名空间调用，在 codegen 中处理
                if (methodCall->args) {
                    for (const auto& arg : methodCall->args->args) { analyzeExpr(*arg); }
                }
                return;
            }
        }
        analyzeExpr(*methodCall->target);
        TypeInfo targetType = evaluateExprType(methodCall->target.get());
        std::string className = targetType.className;

        // 容器方法调用验证
        if (targetType.kind == SymbolKind::Vector) {
            const std::string& mname = methodCall->method->ident;
            size_t argCount = methodCall->args ? methodCall->args->args.size() : 0;
            // 合法方法及参数数量: push(1), pop(0), get(1), set(2), len(0)
            if (mname == "push" && argCount == 1) { /* ok */ }
            else if (mname == "pop" && argCount == 0) { /* ok */ }
            else if (mname == "get" && argCount == 1) { /* ok */ }
            else if (mname == "set" && argCount == 2) { /* ok */ }
            else if (mname == "len" && argCount == 0) { /* ok */ }
            else {
                reportError(*methodCall, "vector 不存在方法或参数数量不匹配：" + mname);
                return;
            }
            if (methodCall->args) {
                for (const auto& arg : methodCall->args->args) { analyzeExpr(*arg); }
            }
            return;
        }
        if (targetType.kind == SymbolKind::Map) {
            const std::string& mname = methodCall->method->ident;
            size_t argCount = methodCall->args ? methodCall->args->args.size() : 0;
            // 合法方法及参数数量: set(2), get(1), contains(1), erase(1), len(0)
            if (mname == "set" && argCount == 2) { /* ok */ }
            else if (mname == "get" && argCount == 1) { /* ok */ }
            else if (mname == "contains" && argCount == 1) { /* ok */ }
            else if (mname == "erase" && argCount == 1) { /* ok */ }
            else if (mname == "len" && argCount == 0) { /* ok */ }
            else {
                reportError(*methodCall, "map 不存在方法或参数数量不匹配：" + mname);
                return;
            }
            if (methodCall->args) {
                for (const auto& arg : methodCall->args->args) { analyzeExpr(*arg); }
            }
            return;
        }
        if (targetType.kind == SymbolKind::Deque) {
            const std::string& mname = methodCall->method->ident;
            size_t argCount = methodCall->args ? methodCall->args->args.size() : 0;
            // 合法方法: push_front(1), push_back(1), pop_front(0), pop_back(0), get(1), set(2), front(0), back(0), len(0)
            if (mname == "push_front" && argCount == 1) { /* ok */ }
            else if (mname == "push_back" && argCount == 1) { /* ok */ }
            else if (mname == "pop_front" && argCount == 0) { /* ok */ }
            else if (mname == "pop_back" && argCount == 0) { /* ok */ }
            else if (mname == "get" && argCount == 1) { /* ok */ }
            else if (mname == "set" && argCount == 2) { /* ok */ }
            else if (mname == "front" && argCount == 0) { /* ok */ }
            else if (mname == "back" && argCount == 0) { /* ok */ }
            else if (mname == "len" && argCount == 0) { /* ok */ }
            else {
                reportError(*methodCall, "deque 不存在方法或参数数量不匹配：" + mname);
                return;
            }
            if (methodCall->args) {
                for (const auto& arg : methodCall->args->args) { analyzeExpr(*arg); }
            }
            return;
        }
        if (targetType.kind == SymbolKind::Queue) {
            const std::string& mname = methodCall->method->ident;
            size_t argCount = methodCall->args ? methodCall->args->args.size() : 0;
            // 合法方法: push(1), pop(0), front(0), back(0), len(0)
            if (mname == "push" && argCount == 1) { /* ok */ }
            else if (mname == "pop" && argCount == 0) { /* ok */ }
            else if (mname == "front" && argCount == 0) { /* ok */ }
            else if (mname == "back" && argCount == 0) { /* ok */ }
            else if (mname == "len" && argCount == 0) { /* ok */ }
            else {
                reportError(*methodCall, "queue 不存在方法或参数数量不匹配：" + mname);
                return;
            }
            if (methodCall->args) {
                for (const auto& arg : methodCall->args->args) { analyzeExpr(*arg); }
            }
            return;
        }
        if (targetType.kind == SymbolKind::Channel) {
            const std::string& mname = methodCall->method->ident;
            size_t argCount = methodCall->args ? methodCall->args->args.size() : 0;
            // 合法方法: send(1), recv(0), len(0), close(0), closed(0)
            if (mname == "send" && argCount == 1) { /* ok */ }
            else if (mname == "recv" && argCount == 0) { /* ok */ }
            else if (mname == "len" && argCount == 0) { /* ok */ }
            else if (mname == "close" && argCount == 0) { /* ok */ }
            else if (mname == "closed" && argCount == 0) { /* ok */ }
            else {
                reportError(*methodCall, "channel 不存在方法或参数数量不匹配：" + mname);
                return;
            }
            if (methodCall->args) {
                for (const auto& arg : methodCall->args->args) { analyzeExpr(*arg); }
            }
            return;
        }
        if (targetType.kind == SymbolKind::String) {
            const std::string& mname = methodCall->method->ident;
            size_t argCount = methodCall->args ? methodCall->args->args.size() : 0;
            // 合法方法: substr(2), find(1), char_at(1), to_upper(0), to_lower(0), replace(2), contains(1)
            if (mname == "substr" && argCount == 2) { /* ok */ }
            else if (mname == "find" && argCount == 1) { /* ok */ }
            else if (mname == "char_at" && argCount == 1) { /* ok */ }
            else if (mname == "to_upper" && argCount == 0) { /* ok */ }
            else if (mname == "to_lower" && argCount == 0) { /* ok */ }
            else if (mname == "replace" && argCount == 2) { /* ok */ }
            else if (mname == "contains" && argCount == 1) { /* ok */ }
            else {
                reportError(*methodCall, "string 不存在方法或参数数量不匹配：" + mname);
                return;
            }
            if (methodCall->args) {
                for (const auto& arg : methodCall->args->args) { analyzeExpr(*arg); }
            }
            return;
        }

        if (targetType.pointerLevel > 0 && targetType.kind == SymbolKind::Class) {
            targetType.pointerLevel -= 1;
        }
        if (className.empty()) {
            className = evaluateExprType(methodCall->target.get()).className;
        }
        if (targetType.kind != SymbolKind::Class || className.empty()) {
            reportError(*methodCall, "方法调用目标不是类类型");
            return;
        }
        SymbolInfo* clsInfo = rootScope ? rootScope->lookup(className) : nullptr;
        if (!clsInfo || clsInfo->kind != SymbolKind::Class) {
            reportError(*methodCall, "未知的类：" + className);
            return;
        }
        auto mit = clsInfo->methodParamTypes.find(methodCall->method->ident);
        if (mit == clsInfo->methodParamTypes.end()) {
            reportError(*methodCall, "方法不存在：" + methodCall->method->ident);
            return;
        }
        const auto& paramTypes = mit->second;
        size_t argCount = methodCall->args ? methodCall->args->args.size() : 0;
        if (argCount != paramTypes.size()) {
            reportError(*methodCall, "方法参数数量不匹配");
        }
        if (methodCall->args) {
            for (size_t i = 0; i < methodCall->args->args.size(); ++i) {
                const auto& arg = methodCall->args->args[i];
                analyzeExpr(*arg);
                if (i < paramTypes.size()) {
                    warnZeroAsNil(*methodCall, paramTypes[i], arg.get());
                }
            }
        }
    } else if (auto newExpr = dynamic_cast<const NewExpr*>(&expr)) {
        const std::string className = newExpr->className->ident;
        auto clsIt = classDecls.find(className);
        if (clsIt == classDecls.end()) {
            reportError(*newExpr, "未知的类：" + className);
            return;
        }

        size_t argCount = newExpr->args ? newExpr->args->args.size() : 0;
        bool hasMatchingCtor = false;
        for (const auto& ctor : clsIt->second->ctors) {
            size_t paramCount = ctor->params ? ctor->params->params.size() : 0;
            if (paramCount == argCount) {
                hasMatchingCtor = true;
                break;
            }
        }
        if (argCount > 0 && !hasMatchingCtor) {
            reportError(*newExpr, "未找到匹配参数数量的构造函数");
        }

        if (newExpr->args) {
            for (const auto& arg : newExpr->args->args) {
                analyzeExpr(*arg);
            }
        }
    } else if (auto newArrExpr = dynamic_cast<const NewArrayExpr*>(&expr)) {
        // new T[n] — 验证大小表达式
        if (newArrExpr->sizeExpr) {
            analyzeExpr(*newArrExpr->sizeExpr);
        } else {
            reportError(*newArrExpr, "new 数组缺少大小表达式");
        }
    } else if (auto strLit = dynamic_cast<const StringLiteralExpr*>(&expr)) {
        // 字符串字面量，无需额外分析
    } else if (auto strlenExpr = dynamic_cast<const StrlenExpr*>(&expr)) {
        analyzeExpr(*strlenExpr->target);
    } else if (dynamic_cast<const ReadlnExpr*>(&expr)) {
        // readln() 无参数，无需额外分析
    } else if (auto itosExpr = dynamic_cast<const ItosExpr*>(&expr)) {
        analyzeExpr(*itosExpr->value);
    } else if (auto tnExpr = dynamic_cast<const TypenameExpr*>(&expr)) {
        analyzeExpr(*tnExpr->target);
    } else if (auto fcExpr = dynamic_cast<const FieldCountExpr*>(&expr)) {
        analyzeExpr(*fcExpr->target);
    } else if (auto mcExpr = dynamic_cast<const MethodCountExpr*>(&expr)) {
        analyzeExpr(*mcExpr->target);
    } else if (auto fnExpr = dynamic_cast<const FieldNameExpr*>(&expr)) {
        analyzeExpr(*fnExpr->target);
        analyzeExpr(*fnExpr->index);
    } else if (auto mnExpr = dynamic_cast<const MethodNameExpr*>(&expr)) {
        analyzeExpr(*mnExpr->target);
        analyzeExpr(*mnExpr->index);
    } else if (auto invokeExpr = dynamic_cast<const InvokeExpr*>(&expr)) {
        analyzeExpr(*invokeExpr->target);
        analyzeExpr(*invokeExpr->methodName);
        if (invokeExpr->args) {
            for (const auto& arg : invokeExpr->args->args) {
                analyzeExpr(*arg);
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

void SemanticAnalyzer::analyzeBoolExpr(BoolExpr& boolExpr)
{
    boolExpr.scope = currentScope;
    if (boolExpr.symbol == "&&" || boolExpr.symbol == "||") {
        // 逻辑二元运算：递归分析两个子布尔表达式
        if (boolExpr.bool_lhs) analyzeBoolExpr(*boolExpr.bool_lhs);
        if (boolExpr.bool_rhs) analyzeBoolExpr(*boolExpr.bool_rhs);
    } else if (boolExpr.symbol == "!") {
        // 逻辑非：递归分析操作数
        if (boolExpr.bool_lhs) analyzeBoolExpr(*boolExpr.bool_lhs);
    } else {
        // 比较运算：分析左右表达式
        if (boolExpr.lhs) analyzeExpr(*boolExpr.lhs);
        if (boolExpr.rhs) analyzeExpr(*boolExpr.rhs);
    }
}

void SemanticAnalyzer::reportError(const ASTNode& node, const std::string& msg)
{
    reportErrorAt(node, "语义分析", msg);
}