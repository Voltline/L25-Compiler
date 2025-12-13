#include "include/ast.h"
#include <llvm/IR/Type.h>
#include <unordered_map>
extern bool hasError;
std::unordered_map<std::string, int> functionMap;

// TypeInfo 方法
TypeInfo::TypeInfo()
    : kind(SymbolKind::Invalid), dims() {}

TypeInfo::TypeInfo(SymbolKind kind, std::vector<int> dims)
    : kind(kind), dims(std::move(dims)) {}

// ASTNode 方法
void ASTNode::reportError(const std::string& msg) const
{
    std::cerr 
        << "\033[1;31m[代码生成错误]\033[0m "
        << "位于 \033[1;33m第 " << lineno 
        << " 行, 第 " << column << " 列\033[0m: "
        << msg << std::endl;
    hasError = true;
}

// 程序节点
Program::Program(std::unique_ptr<IdentExpr> name, std::vector<std::unique_ptr<Func>> functions, std::unique_ptr<StmtList> main_body)
    : name(std::move(name))
    , functions(std::move(functions))
    , main_body(std::move(main_body)) {}

void Program::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Program(" << *name << ")" << std::endl;
    for (const auto& func: functions) {
        func->print(indent + 2);
    }
    main_body->print(indent + 2);
}

llvm::Value* Program::codeGen(CodeGenContext& ctx) const  
{
    // 声明printf函数，用于后续的输出
    if (!ctx.module.getFunction("printf")) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true // 可变参数
        );
        llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", ctx.module);
    }

    // 声明scanf函数，用于后续的输入
    if (!ctx.module.getFunction("scanf")) {
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true // 可变参数
        );
        llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage, "scanf", ctx.module);
    }

    // 函数部分
    for (auto& function: functions) {
        function->codeGen(ctx);
    }

    // main函数
    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx.context), false);
    llvm::Function* mainFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", ctx.module);

    // 入口基本块
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx.context, "entry", mainFunc);
    ctx.builder.SetInsertPoint(entry);

    // 生成main_body的IR
    main_body->codeGen(ctx);

    // 添加默认返回
    ctx.builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
    return mainFunc;
}

// 函数节点
Func::Func(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> stmts, std::unique_ptr<Expr> return_value)
    : name(std::move(name))
    , params(std::move(params))
    , stmts(std::move(stmts))
    , return_value(std::move(return_value)) {}

void Func::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Func" << std::endl;
    name->print(indent + 2);
    if (params) {
        params->print(indent + 2);
    }
    stmts->print(indent + 2);
    return_value->print(indent + 2);
}

llvm::Value* Func::codeGen(CodeGenContext& ctx) const  
{
    const std::string funcName= name->ident;

    SymbolInfo* funcSymbol = scope->lookup(funcName);
    if (!funcSymbol) {
        reportError("函数符号 " + funcName + " 不存在");
        return nullptr;
    }

    std::vector<llvm::Type*> argTypes;
    for (const auto& typeInfo: funcSymbol->paramTypes) {
        if (typeInfo.kind == SymbolKind::Array) {
            // 数组退化成指针
            llvm::Type* elementType = llvm::Type::getInt32Ty(ctx.context);
            llvm::Type* arrayType = elementType;
            for (int i = typeInfo.dims.size() - 1; i >= 0; i--) {
                arrayType = llvm::ArrayType::get(arrayType, typeInfo.dims[i]);
            }
            argTypes.push_back(llvm::PointerType::get(arrayType, 0));
        } else {
            argTypes.push_back(llvm::Type::getInt32Ty(ctx.context));
        }
    }

    // 隐式捕获参数（以指针方式传递）
    for (auto* captured : captures) {
        if (!captured) continue;
        if (captured->kind == SymbolKind::Array) {
            llvm::Type* elementType = llvm::Type::getInt32Ty(ctx.context);
            llvm::Type* arrayType = elementType;
            for (int i = captured->dimensions.size() - 1; i >= 0; i--) {
                arrayType = llvm::ArrayType::get(arrayType, captured->dimensions[i]);
            }
            argTypes.push_back(llvm::PointerType::get(arrayType, 0));
        } else {
            argTypes.push_back(llvm::PointerType::get(llvm::Type::getInt32Ty(ctx.context), 0));
        }
    }
    
    if (!functionMap.contains(funcName)) {
        functionMap[funcName] = 1;
        funcSymbol->llvmName = funcName;
    } else {
        std::string tmpName = funcName + "." + std::to_string(functionMap[funcName]);
        funcSymbol->llvmName = tmpName;
        functionMap[funcName]++;
    }

    llvm::FunctionType* funcType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(ctx.context), argTypes, false
    );

    std::string funcLLVMName = funcSymbol->llvmName;
    llvm::Function* function = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, funcLLVMName, ctx.module
    );

    funcSymbol->value = function; // 更新函数指针

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx.context, "entry", function);
    ctx.builder.SetInsertPoint(entry);

    std::vector<std::pair<SymbolInfo*, llvm::Value*>> capturedOriginalAddrs;

    int idx = 0;
    for (auto& arg: function->args()) {
        if (params && idx < static_cast<int>(params->params.size())) {
            arg.setName(params->params[idx]->ident); // 设置形参名
            SymbolInfo* argInfo = body_scope->lookupLocal(params->params[idx]->ident);

            if (argInfo->kind == SymbolKind::Array) {
                // 数组：直接把传入的指针存储在 alloca 里
                argInfo->addr = &arg;
            } else {
                llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
                ctx.builder.CreateStore(&arg, alloca);
                argInfo->addr = alloca;
            }
            argInfo->value = &arg;
        } else {
            int captureIndex = idx - (params ? static_cast<int>(params->params.size()) : 0);
            if (captureIndex >= 0 && captureIndex < static_cast<int>(captures.size())) {
                SymbolInfo* capturedInfo = captures[captureIndex];
                std::string captureName = capturedInfo ? capturedInfo->name + "_capture" : "capture";
                arg.setName(captureName);
                if (capturedInfo) {
                    capturedOriginalAddrs.emplace_back(capturedInfo, capturedInfo->addr);
                    capturedInfo->addr = &arg;
                    capturedInfo->value = &arg;
                }
            }
        }
        idx++;
    }

    for (const auto& stmt: stmts->stmts) {
        if (dynamic_cast<const Func*>(stmt.get())) {
            llvm::IRBuilder<>::InsertPoint savedIP = ctx.builder.saveIP();
            stmt->codeGen(ctx);
            ctx.builder.restoreIP(savedIP);
        } else {
            stmt->codeGen(ctx);
        }
    }

    llvm::Value* retVal = return_value->codeGen(ctx);
    ctx.builder.CreateRet(retVal);

    // 生成完毕后恢复捕获符号的原始地址
    for (auto& [symbol, originalAddr] : capturedOriginalAddrs) {
        symbol->addr = originalAddr;
    }

    return function;
}

// 语句列表节点
StmtList::StmtList(std::vector<std::unique_ptr<Stmt>> stmts)
    : stmts(std::move(stmts)) {}

void StmtList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "StmtList" << std::endl;
    for (const auto& stmt: stmts) {
        stmt->print(indent + 2);
    }
}

llvm::Value* StmtList::codeGen(CodeGenContext& ctx) const 
{
    llvm::Value* last = nullptr;

    ctx.currentBlock = ctx.builder.GetInsertBlock(); // 初始插入点

    for (auto& stmt : stmts) {
        if (ctx.currentBlock->getTerminator()) {
            // 该块已终结，不再插入后续语句
            break;
        }

        ctx.builder.SetInsertPoint(ctx.currentBlock); // 确保插入点正确

        if (auto* funcDefStmt = dynamic_cast<Func*>(stmt.get())) {
            auto* oldInsertPoint = ctx.builder.GetInsertBlock();
            last = funcDefStmt->codeGen(ctx);
            ctx.builder.SetInsertPoint(oldInsertPoint); // 恢复现场
        } else {
            last = stmt->codeGen(ctx);
        }

        // 更新 currentBlock（避免插入到死块）
        ctx.currentBlock = ctx.builder.GetInsertBlock();
    }

    return last;
}

// 声明语句节点
DeclareStmt::DeclareStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<Expr> expr)
    : name(std::move(name)), expr(std::move(expr)) {}

void DeclareStmt::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Declare" << std::endl;
    name->print(indent + 2);
    if (expr) {
        expr->print(indent + 2);
    }
}

llvm::Value* DeclareStmt::codeGen(CodeGenContext& ctx) const  
{
    llvm::Type* intType = llvm::Type::getInt32Ty(ctx.context);
    llvm::AllocaInst* alloca = nullptr;

    const std::string& ident_name = name->ident;
    auto typeInfo = name->type;

    if (typeInfo.kind == SymbolKind::Int) {
        // 普通整形的分配
        alloca = ctx.builder.CreateAlloca(intType, nullptr, ident_name);
        // 赋初值0 
        if (!expr) {
            llvm::Value* zeroInit = llvm::ConstantInt::get(intType, 0);
            ctx.builder.CreateStore(zeroInit, alloca);
        }
    } else if (typeInfo.kind == SymbolKind::Array) {
        // 多维数组的分配
        llvm::Type* arrayType = intType;
        for (auto it{ typeInfo.dims.rbegin() }; it != typeInfo.dims.rend(); it++) {
            arrayType = llvm::ArrayType::get(arrayType, *it);
        }
        alloca = ctx.builder.CreateAlloca(arrayType, nullptr, ident_name);

        // 类型参数列表：i8* 和 i64
        auto memsetFn = llvm::Intrinsic::getDeclaration(
            &ctx.module,
            llvm::Intrinsic::memset,
            {
                llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                llvm::Type::getInt64Ty(ctx.context)
            }
        );

        // 构造参数
        llvm::Value* zeroVal = llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::Value* sizeVal = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(ctx.context),
            ctx.module.getDataLayout().getTypeAllocSize(arrayType)
        );
        llvm::Value* isVolatile = llvm::ConstantInt::getFalse(ctx.context);

        // 调用 memset
        ctx.builder.CreateCall(memsetFn, {
            ctx.builder.CreateBitCast(alloca, llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0)),
            zeroVal,
            sizeVal,
            isVolatile
        });
    }

    if (!alloca) {
        reportError("无法为变量: " + ident_name + " 分配空间");
        return nullptr;
    }

    // 找到符号
    SymbolInfo* symbolInfo = scope->lookupLocal(ident_name);
    symbolInfo->addr = alloca;

    // 存在赋值
    if (expr) {
        llvm::Value* initVal = expr->codeGen(ctx);
        if (initVal) {
            // 普通变量直接存入
            if (typeInfo.kind == SymbolKind::Int) {
                ctx.builder.CreateStore(initVal, alloca);
            }
        }
    }
    return alloca;
}

// 赋值语句节点
AssignStmt::AssignStmt(std::unique_ptr<Expr> name, std::unique_ptr<Expr> expr)
    : name(std::move(name)), expr(std::move(expr)) {}

void AssignStmt::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Assign" << std::endl;
    name->print(indent + 2);
    expr->print(indent + 2);
}

llvm::Value* AssignStmt::codeGen(CodeGenContext& ctx) const  
{
    llvm::Value* rhs = expr->codeGen(ctx);
    if (!rhs) {
        reportError("赋值右侧表达式生成失败");
        return nullptr;
    }

    llvm::Value* lhsAddr = nullptr;
    
    // 普通变量
    if (auto identExpr = dynamic_cast<IdentExpr*>(name.get())) {
        SymbolInfo* symbol = scope->lookup(identExpr->ident);
        if (!symbol || !symbol->addr) {
            reportError("变量未声明或未分配空间: " + identExpr->ident);
            return nullptr;
        }
        lhsAddr = symbol->addr;
    } else if (auto arrayExpr = dynamic_cast<ArraySubscriptExpr*>(name.get())) {
        lhsAddr = arrayExpr->getAddress(ctx);
        if (!lhsAddr) {
            reportError("获取数组元素地址失败");
            return nullptr;
        }
    } else {
        reportError("左值类型错误");
        return nullptr;
    }

    if (!lhsAddr) {

    }
    ctx.builder.CreateStore(rhs, lhsAddr);
    return rhs;
}

// 条件分支语句节点
IfStmt::IfStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> if_body, std::unique_ptr<StmtList> else_body)
    : condition(std::move(condition))
    , if_body(std::move(if_body))
    , else_body(std::move(else_body)) {}

void IfStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "if" << std::endl;
    condition->print(indent + 2);
    if_body->print(indent + 2);
    if (else_body) {
        else_body->print(indent + 2);
    }
}

llvm::Value* IfStmt::codeGen(CodeGenContext& ctx) const  
{
    llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

    llvm::Value* condValue = condition->codeGen(ctx);
    if (!condValue) {
        reportError("if条件表达式生成失败");
        return nullptr;
    }

    // 创建块
    llvm::BasicBlock* ifBody = llvm::BasicBlock::Create(ctx.context, "if.then", function);
    llvm::BasicBlock* elseBody = else_body ? llvm::BasicBlock::Create(ctx.context, "if.else", function) : nullptr;
    llvm::BasicBlock* merge = llvm::BasicBlock::Create(ctx.context, "if.end", function);

    // 条件跳转
    if (elseBody) {
        ctx.builder.CreateCondBr(condValue, ifBody, elseBody);
    } else {
        ctx.builder.CreateCondBr(condValue, ifBody, merge);
    }

    // if-body
    ctx.builder.SetInsertPoint(ifBody);
    ctx.currentBlock = ifBody;
    if_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        ctx.builder.CreateBr(merge);
    }

    // else-body
    if (elseBody) {
        ctx.builder.SetInsertPoint(elseBody);
        ctx.currentBlock = elseBody;
        else_body->codeGen(ctx);

        if (!ctx.currentBlock->getTerminator()) {
            ctx.builder.SetInsertPoint(ctx.currentBlock);
            ctx.builder.CreateBr(merge);
        }
    }

    // 合并块
    ctx.builder.SetInsertPoint(merge);
    ctx.currentBlock = merge;

    return nullptr;
}

// While循环语句节点
WhileStmt::WhileStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> loop_body)
    : condition(std::move(condition))
    , loop_body(std::move(loop_body)) {}

void WhileStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "while" << std::endl;
    condition->print(indent + 2);
    loop_body->print(indent + 2);
}

llvm::Value* WhileStmt::codeGen(CodeGenContext& ctx) const 
{
    llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

    // 创建基本块
    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(ctx.context, "while.cond", function);
    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(ctx.context, "while.body", function);
    llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(ctx.context, "while.after", function);

    // 创建跳转到 condBlock，连上 while 结构
    ctx.builder.CreateBr(condBlock);

    // condBlock
    ctx.builder.SetInsertPoint(condBlock);     // 设置插入点
    ctx.currentBlock = condBlock;

    llvm::Value* condValue = condition->codeGen(ctx);
    if (!condValue) return nullptr;

    if (!condValue->getType()->isIntegerTy(1)) {
        condValue = ctx.builder.CreateICmpNE(condValue, llvm::ConstantInt::get(condValue->getType(), 0), "whilecond");
    }

    ctx.builder.CreateCondBr(condValue, bodyBlock, afterBlock);

    // bodyBlock
    ctx.builder.SetInsertPoint(bodyBlock);
    ctx.currentBlock = bodyBlock;

    loop_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        ctx.builder.CreateBr(condBlock);
    }

    // 必须插入 afterBlock，不然后续的代码可能跳不到这里
    ctx.builder.SetInsertPoint(afterBlock);
    ctx.currentBlock = afterBlock;

    return nullptr;
}

// 函数调用语句节点
FuncCallStmt::FuncCallStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<ArgList> args)
    : name(std::move(name)), args(std::move(args)) {}

void FuncCallStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Call" << std::endl;
    name->print(indent + 2);
    if (args) {
        args->print(indent + 2);
    }
}

llvm::Value* FuncCallStmt::codeGen(CodeGenContext& ctx) const  
{
    const std::string& funcName = name->ident;

    SymbolInfo* funcSymbol = scope->lookup(funcName);
    if (!funcSymbol || funcSymbol->kind != SymbolKind::Function) {
        reportError("函数: " + funcName + " 未定义");
        return nullptr;
    }
    std::string funcLLVMName = funcSymbol->llvmName;
    llvm::Function* calleeFunc = ctx.module.getFunction(funcLLVMName);

    std::vector<llvm::Value*> argsV;
    if (args) {
        for (const auto& argExpr: args->args) {
            llvm::Value* argVal = argExpr->codeGen(ctx);
            if (!argVal) return nullptr;
            argsV.push_back(argVal);
        }
    }

    if (funcSymbol->funcDef) {
        for (auto* captured : funcSymbol->funcDef->captures) {
            if (!captured) continue;
            SymbolInfo* callerSymbol = scope->lookup(captured->name);
            if (!callerSymbol || !callerSymbol->addr) {
                reportError("捕获变量: " + captured->name + " 在调用点不可用");
                return nullptr;
            }
            argsV.push_back(callerSymbol->addr);
        }
    }

    ctx.builder.CreateCall(calleeFunc, argsV);
    return nullptr; // 作为语句，不返回值
}

// 输入语句节点
InputStmt::InputStmt(std::vector<std::unique_ptr<Expr>> idents)
    : idents(std::move(idents)) {}

InputStmt::InputStmt(std::unique_ptr<InputArgList> args)
    : idents(std::move(args->idents)) {}

void InputStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Input" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* InputStmt::codeGen(CodeGenContext& ctx) const  
{
    llvm::Function* scanfFunc = ctx.module.getFunction("scanf");
    if (!scanfFunc) {
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true
        );
        scanfFunc = llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage, "scanf", ctx.module);
    }

    llvm::Value* formatStr = ctx.builder.CreateGlobalString("%d");

    for (auto& ident: idents) {
        assert(scope && "InputStmt::codeGen 中的 scope 为空");
        llvm::Value* addr = nullptr;
        if (auto* idExpr = dynamic_cast<IdentExpr*>(ident.get())) {
            SymbolInfo* symbol = scope->lookup(idExpr->ident);
            if (!symbol) {
                reportError("变量: " + idExpr->ident + " 未声明");
                return nullptr;
            }

            if (symbol->kind == SymbolKind::Array) {
                reportError("不支持直接输入数组: " + idExpr->ident);
                continue;
            }

            if (!symbol->addr) {
                reportError("变量: " + idExpr->ident + " 未分配空间");
                return nullptr;
            }
            addr = symbol->addr;
        } else if (auto* arraySubscriptExpr = dynamic_cast<ArraySubscriptExpr*>(ident.get())) {
            addr = arraySubscriptExpr->getAddress(ctx);
            if (!addr) {
                reportError("数组下标访问异常");
            }
        }
        ctx.builder.CreateCall(scanfFunc, { formatStr, addr });
    }
    return nullptr;
}

// 输出语句节点
OutputStmt::OutputStmt(std::vector<std::unique_ptr<Expr>> idents)
    : idents(std::move(idents)) {}

OutputStmt::OutputStmt(std::unique_ptr<ArgList> args)
    : idents(std::move(args->args)) {}

void OutputStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Output" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* OutputStmt::codeGen(CodeGenContext& ctx) const {
    // 获取或声明 printf 函数
    llvm::Function* printfFunc = ctx.module.getFunction("printf");
    if (!printfFunc) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true // 可变参数
        );
        printfFunc = llvm::Function::Create(
            printfType,
            llvm::Function::ExternalLinkage,
            "printf",
            ctx.module
        );
    }

    // 构建格式字符串和参数列表
    std::string formatStr;
    std::vector<llvm::Value*> printfArgs;

    for (const auto& expr : idents) {
        llvm::Value* val = expr->codeGen(ctx);
        if (val) {
            if (!formatStr.empty()) {
                formatStr += " "; // 多个数之间空格分隔
            }
            formatStr += "%d";
            printfArgs.push_back(val);
        }
    }

    if (!formatStr.empty()) {
        formatStr += "\n"; // 行末换行
        llvm::Value* formatStrVal = ctx.builder.CreateGlobalString(formatStr);
        printfArgs.insert(printfArgs.begin(), formatStrVal); // 格式串是第一个参数
        ctx.builder.CreateCall(printfFunc, printfArgs);
    }

    return nullptr;
}

// Bool表达式节点
BoolExpr::BoolExpr(std::string symbol, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
    : symbol(symbol)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs)) {}

void BoolExpr::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Bool(" << symbol << ")" << std::endl;
    lhs->print(indent + 2);
    rhs->print(indent + 2);
}

llvm::Value* BoolExpr::codeGen(CodeGenContext& ctx) const  
{
    llvm::Value* lhsVal = lhs->codeGen(ctx);
    llvm::Value* rhsVal = rhs->codeGen(ctx);

    if (!lhsVal || !rhsVal) {
        reportError("布尔表达式左右子表达式生成失败");
        return nullptr;
    }

    if (lhsVal->getType()->isIntegerTy() && rhsVal->getType()->isIntegerTy()) {
        if (symbol == "==") {
            return ctx.builder.CreateICmpEQ(lhsVal, rhsVal, "cmpeq");
        } else if (symbol == "!=") {
            return ctx.builder.CreateICmpNE(lhsVal, rhsVal, "cmpne");
        } else if (symbol == "<") {
            return ctx.builder.CreateICmpSLT(lhsVal, rhsVal, "cmplt");
        } else if (symbol == "<=") {
            return ctx.builder.CreateICmpSLE(lhsVal, rhsVal, "cmple");
        } else if (symbol == ">") {
            return ctx.builder.CreateICmpSGT(lhsVal, rhsVal, "cmpgt");
        } else if (symbol == ">=") {
            return ctx.builder.CreateICmpSGE(lhsVal, rhsVal, "cmpge");
        } else {
            reportError("不支持的布尔操作符");
            return nullptr;
        }
    } else {
        reportError("不支持非整数类型的布尔比较");
        return nullptr;
    }
}

// 整数常量节点
NumberExpr::NumberExpr(int val) : value(val) {}

void NumberExpr::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Number(" << value << ")" << std::endl;
}

llvm::Value* NumberExpr::codeGen(CodeGenContext& ctx) const 
{
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), value);
}

// 一元运算符节点
UnaryExpr::UnaryExpr(char op, std::unique_ptr<Expr> rhs): op(op), rhs(std::move(rhs)) {}

void UnaryExpr::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Unary(" << op << ")" << std::endl;
    rhs->print(indent+2);
}

llvm::Value* UnaryExpr::codeGen(CodeGenContext& ctx) const 
{
    llvm::Value* RHS = rhs->codeGen(ctx);
    switch (op) {
    case '+':
        return RHS;
    case '-':
        return ctx.builder.CreateNeg(RHS);
    default:
        return nullptr;
    }
}

// 二元运算符节点
BinaryExpr::BinaryExpr(char op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

void BinaryExpr::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Binary(" << op << ")" << std::endl;
    lhs->print(indent + 2);
    rhs->print(indent + 2);
}

llvm::Value* BinaryExpr::codeGen(CodeGenContext& ctx) const 
{
    llvm::Value* LHS = lhs->codeGen(ctx);
    llvm::Value* RHS = rhs->codeGen(ctx);
    if (!LHS || !RHS) {
        reportError("二元运算的子表达式生成失败");
        return nullptr;
    }

    if (!LHS->getType()->isIntegerTy(32) || !RHS->getType()->isIntegerTy(32)) {
        reportError("非整数元素不能参与二元运算");
        return nullptr;
    }

    switch (op) {
    case '+':
        return ctx.builder.CreateAdd(LHS, RHS, "addtmp");
    case '-':
        return ctx.builder.CreateSub(LHS, RHS, "subtmp");
    case '*':
        return ctx.builder.CreateMul(LHS, RHS, "multmp");
    case '/':
        return ctx.builder.CreateSDiv(LHS, RHS, "divtmp");
    case '%':
        return ctx.builder.CreateSRem(LHS, RHS, "modtmp");
    default:
        reportError("不支持的二元运算符: " + std::string(1, op));
        return nullptr;
    }
}

// 数组下标访问运算节点
ArraySubscriptExpr::ArraySubscriptExpr(std::unique_ptr<IdentExpr> array, std::vector<std::unique_ptr<Expr>> subscript)
    : array(std::move(array)), subscript(std::move(subscript)) {}

void ArraySubscriptExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Array Subscript(" << array->ident << "[" << std::endl;
    for (int i = 0; i < subscript.size(); i++) {
        subscript[i]->print(indent + 2);
    }
    std::cout << std::string(indent, ' ') << "])" << std::endl;
}

llvm::Value* ArraySubscriptExpr::codeGen(CodeGenContext& ctx) const {
    SymbolInfo* symbol = scope->lookup(array->ident);
    if (!symbol) {
        reportError("数组: " + array->ident + " 未声明");
        return nullptr;
    }

    llvm::Value* arrayAlloca = symbol->addr;
    llvm::Value* arrayPtr = nullptr;

    // 构造数组的完整类型：[d1 x [d2 x ... [dn x i32]]]
    llvm::Type* elementType = llvm::Type::getInt32Ty(ctx.context);
    for (int i = symbol->dimensions.size() - 1; i >= 0; --i) {
        elementType = llvm::ArrayType::get(elementType, symbol->dimensions[i]);
    }

    if (symbol->isFuncParam) {
        // 函数参数情况，形如：alloca ptr -> store ptr to array
        arrayPtr = ctx.builder.CreateLoad(llvm::PointerType::get(elementType, 0), arrayAlloca, array->ident + "_loaded");
    } else {
        // 本地变量（alloca 的就是数组）
        arrayPtr = arrayAlloca;
    }

    if (!arrayPtr) {
        reportError("数组: " + array->ident + " 未分配空间");
        return nullptr;
    }

    // 计算下标
    std::vector<llvm::Value*> indices;
    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
    for (auto& expr: subscript) {
        llvm::Value* index = expr->codeGen(ctx);
        if (index) {
            indices.push_back(index);
        }
    }

    if (indices.size() - 1 != symbol->dimensions.size()) {
        reportError("数组维度不匹配，无法访问: " + std::to_string(indices.size() - 1) + " != " + std::to_string(symbol->dimensions.size()));
        return nullptr;
    }

    llvm::Value* gep = ctx.builder.CreateGEP(
        elementType,
        arrayPtr,
        indices,
        "array_elem"
    );

    return ctx.builder.CreateLoad(llvm::Type::getInt32Ty(ctx.context), gep, "load_elem");
}

llvm::Value* ArraySubscriptExpr::getAddress(CodeGenContext& ctx) const {
    SymbolInfo* symbol = scope->lookup(array->ident);
    if (!symbol) {
        reportError("数组: " + array->ident + " 未声明");
        return nullptr;
    }

    llvm::Value* arrayAlloca = symbol->addr;
    llvm::Value* arrayPtr = nullptr;

    // 构造完整数组类型
    llvm::Type* elementType = llvm::Type::getInt32Ty(ctx.context);
    for (int i = symbol->dimensions.size() - 1; i >= 0; --i) {
        elementType = llvm::ArrayType::get(elementType, symbol->dimensions[i]);
    }

    if (symbol->isFuncParam) {
        arrayPtr = ctx.builder.CreateLoad(llvm::PointerType::get(elementType, 0), arrayAlloca, array->ident + "_loaded");
    } else {
        arrayPtr = arrayAlloca;
    }

    if (!arrayPtr) {
        reportError("数组: " + array->ident + " 未分配空间");
        return nullptr;
    }

    // 构造 GEP 索引
    std::vector<llvm::Value*> indices;
    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
    for (const auto& expr : subscript) {
        llvm::Value* idxVal = expr->codeGen(ctx);
        if (!idxVal) {
            reportError("存在无法作为下标的符号");
            return nullptr;  // 错误处理
        }
        indices.push_back(idxVal);
    }

    llvm::Value* gep = ctx.builder.CreateGEP(
        elementType,
        arrayPtr,
        indices,
        "array_elem"
    );

    return gep;
}

// 标识符节点
IdentExpr::IdentExpr(const std::string& ident, TypeInfo type) : ident(ident), type(type) {}

void IdentExpr::print(int indent) const 
{
    switch (type.kind) {
    case SymbolKind::Int:
    case SymbolKind::Function:
    case SymbolKind::Program:
        std::cout << std::string(indent, ' ') 
            << "Ident(" << ident << ": " 
            << SymbolName[static_cast<int>(type.kind)] << ")" << std::endl;
        break;
    case SymbolKind::Array:
        std::cout << std::string(indent, ' ') 
            << "Ident(" << ident << ": Array[";
        for (int i = 0; i < type.dims.size(); i++) {
            if (i != 0) std::cout << ",";
            std::cout << type.dims[i];
        }
        std::cout << "])" << std::endl;
        break;
    default:
        std::cout << std::string(indent, ' ') 
            << "Ident(" << ident << ")" << std::endl;
    }
}

llvm::Value* IdentExpr::codeGen(CodeGenContext& ctx) const 
{
    SymbolInfo* symbol = scope->lookup(ident);

    if (!symbol) {
        reportError("标识符: " + ident + " 不存在");
        return nullptr;
    }

    // 如果为变量
    if (symbol->kind == SymbolKind::Int) {
        llvm::Type* valueType = llvm::Type::getInt32Ty(ctx.context);
        return ctx.builder.CreateLoad(valueType, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::Array) {
        return symbol->addr;
    }

    reportError("不支持返回的标识符: " + ident);
    return nullptr;
}

// 函数调用表达式节点
FuncCallExpr::FuncCallExpr(std::unique_ptr<IdentExpr> name, std::unique_ptr<ArgList> args)
    : name(std::move(name)), args(std::move(args)) {}

FuncCallExpr::FuncCallExpr(std::unique_ptr<FuncCallStmt> funcCallStmt) 
    : name(std::move(funcCallStmt->name)), args(std::move(funcCallStmt->args)) {}

void FuncCallExpr::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Call" << std::endl;
    name->print(indent + 2);
    if (args) {
        args->print(indent + 2);
    }
}

llvm::Value* FuncCallExpr::codeGen(CodeGenContext& ctx) const  
{
    const std::string& funcName = name->ident;

    SymbolInfo* funcSymbol = scope->lookup(funcName);
    if (!funcSymbol || funcSymbol->kind != SymbolKind::Function) {
        reportError("函数: " + funcName + " 未定义");
        return nullptr;
    }
    std::string funcLLVMName = funcSymbol->llvmName;
    llvm::Function* calleeFunc = ctx.module.getFunction(funcLLVMName);

    std::vector<llvm::Value*> argsV;
    if (args) {
        for (const auto& argExpr: args->args) {
            llvm::Value* argVal = argExpr->codeGen(ctx);
            if (!argVal) return nullptr;
            argsV.push_back(argVal);
        }
    }

    if (funcSymbol->funcDef) {
        for (auto* captured : funcSymbol->funcDef->captures) {
            if (!captured) continue;
            SymbolInfo* callerSymbol = scope->lookup(captured->name);
            if (!callerSymbol || !callerSymbol->addr) {
                reportError("捕获变量: " + captured->name + " 在调用点不可用");
                return nullptr;
            }
            argsV.push_back(callerSymbol->addr);
        }
    }

    return ctx.builder.CreateCall(calleeFunc, argsV, funcName + "_call");
}

// 参数列表节点
ArgList::ArgList(std::vector<std::unique_ptr<Expr>> args)
    : args(std::move(args)) {}

void ArgList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "ArgList" << std::endl;
    for (const auto& arg: args) {
        arg->print(indent + 2);
    }
}

llvm::Value* ArgList::codeGen(CodeGenContext& ctx) const 
{
    return nullptr;
}

// 形式参数列表节点
ParamList::ParamList(std::vector<std::unique_ptr<IdentExpr>> params)
    : params(std::move(params)) {}

void ParamList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "ParamList" << std::endl;
    for (const auto& param: params) {
        param->print(indent + 2);
    }
}

llvm::Value* ParamList::codeGen(CodeGenContext& ctx) const 
{
    return nullptr;
}

// 输入函数参数列表节点
InputArgList::InputArgList(std::vector<std::unique_ptr<Expr>> idents)
    : idents(std::move(idents)) {}

void InputArgList::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Input Args" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* InputArgList::codeGen(CodeGenContext& ctx) const
{
    return nullptr;
}