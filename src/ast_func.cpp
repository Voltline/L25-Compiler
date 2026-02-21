#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

// ===== 函数节点 =====
Func::Func(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> stmts, std::unique_ptr<Expr> return_value, TypeInfo returnType)
    : name(std::move(name))
    , params(std::move(params))
    , stmts(std::move(stmts))
    , return_value(std::move(return_value))
    , returnType(std::move(returnType)) {}

void Func::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Func" << std::endl;
    name->print(indent + 2);
    if (params) {
        params->print(indent + 2);
    }
    stmts->print(indent + 2);
    if (return_value) {
        return_value->print(indent + 2);
    }
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
        llvm::Type* argType = nullptr;
        if (typeInfo.kind == SymbolKind::Array) {
            argType = typeInfoToLLVMType(typeInfo, ctx.context, true);
        } else {
            argType = typeInfoToLLVMValueType(typeInfo, ctx.context);
        }
        argTypes.push_back(argType);
    }

    // 隐式捕获参数（以指针方式传递）
    for (auto* captured : captures) {
        if (!captured) continue;
        if (captured->kind == SymbolKind::Array) {
            TypeInfo captureType{ SymbolKind::Array, captured->dimensions, 1, captured->isFloat };
            argTypes.push_back(typeInfoToLLVMType(captureType, ctx.context, true));
        } else {
            TypeInfo captureType{ captured->kind, {}, captured->pointerLevel + 1, captured->isFloat };
            argTypes.push_back(typeInfoToLLVMValueType(captureType, ctx.context));
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

    TypeInfo retTypeInfo = funcSymbol->returnType.kind == SymbolKind::Invalid ? returnType : funcSymbol->returnType;
    llvm::Type* retLLVMType = typeInfoToLLVMValueType(retTypeInfo, ctx.context);
    if (!retLLVMType) retLLVMType = llvm::Type::getInt32Ty(ctx.context);

    llvm::FunctionType* funcType = llvm::FunctionType::get(
        retLLVMType, argTypes, false
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
                argInfo->isFuncParam = true;
                auto* placeholder = ctx.builder.CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
                ctx.builder.CreateStore(&arg, placeholder);
                argInfo->addr = placeholder;
            } else {
                llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
                ctx.builder.CreateStore(&arg, alloca);
                argInfo->addr = alloca;
                // GC: 类指针参数注册为根
                if (arg.getType()->isPointerTy() && params->params[idx]->type.kind == SymbolKind::Class
                    && params->params[idx]->type.pointerLevel > 0) {
                    ensureGCRuntimeDeclared(ctx);
                    auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
                    auto* i8PtrPtrTy = llvm::PointerType::get(i8PtrTy, 0);
                    llvm::Value* rootAddr = ctx.builder.CreateBitCast(alloca, i8PtrPtrTy, arg.getName() + ".root");
                    ctx.builder.CreateCall(ctx.module.getFunction("l25_gc_add_root"), {rootAddr});
                    ctx.registerCleanup(alloca, CleanupKind::GCRoot);
                }
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

    ctx.pushCleanupScope();
    for (const auto& stmt: stmts->stmts) {
        if (dynamic_cast<const Func*>(stmt.get())) {
            llvm::IRBuilder<>::InsertPoint savedIP = ctx.builder.saveIP();
            stmt->codeGen(ctx);
            ctx.builder.restoreIP(savedIP);
        } else {
            stmt->codeGen(ctx);
        }
    }

    llvm::Value* retVal = return_value ? return_value->codeGen(ctx) : defaultValueForType(retTypeInfo, ctx);

    // 如果返回字符串变量，先将其数据置空以阻止清理释放返回值
    if (return_value) {
        if (retTypeInfo.kind == SymbolKind::String && retTypeInfo.pointerLevel == 0) {
            if (auto* identRet = dynamic_cast<IdentExpr*>(return_value.get())) {
                SymbolInfo* sym = body_scope->lookup(identRet->ident);
                if (sym && sym->addr) {
                    llvm::StructType* strTy = getL25StringType(ctx.context);
                    ctx.builder.CreateStore(llvm::ConstantAggregateZero::get(strTy), sym->addr);
                }
            } else if (!isOwnedStringExpr(return_value.get())) {
                // 非变量且非拥有型表达式（如字符串字面量），深拷贝保证返回拥有权缓冲区
                retVal = emitStringDeepCopy(retVal, ctx);
            }
        } else if (retTypeInfo.kind == SymbolKind::Class && retTypeInfo.pointerLevel > 0) {
            if (auto* identRet = dynamic_cast<IdentExpr*>(return_value.get())) {
                SymbolInfo* sym = body_scope->lookup(identRet->ident);
                if (sym && sym->addr) {
                    llvm::Type* ptrTy = retVal->getType();
                    ctx.builder.CreateStore(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(ptrTy)), sym->addr);
                }
            }
        }
    }

    emitScopeCleanup(ctx);
    retVal = castValueToType(retVal, retLLVMType, ctx);
    ctx.builder.CreateRet(retVal);

    // 生成完毕后恢复捕获符号的原始地址
    for (auto& [symbol, originalAddr] : capturedOriginalAddrs) {
        symbol->addr = originalAddr;
    }

    return function;
}

// ===== 函数调用语句节点 =====
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
        int idx = 0;
        for (const auto& argExpr: args->args) {
            llvm::Value* argVal = argExpr->codeGen(ctx);
            if (!argVal) return nullptr;

            if (idx < static_cast<int>(funcSymbol->paramTypes.size())) {
                const TypeInfo& expected = funcSymbol->paramTypes[idx];
                llvm::Type* expectedType = expected.kind == SymbolKind::Array
                    ? typeInfoToLLVMType(expected, ctx.context, true)
                    : typeInfoToLLVMValueType(expected, ctx.context);
                argVal = castValueToType(argVal, expectedType, ctx);
            }
            argsV.push_back(argVal);
            idx++;
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

// ===== 表达式语句节点 =====
void ExprStmt::print(int indent) const
{
    if (expr) expr->print(indent);
}

llvm::Value* ExprStmt::codeGen(CodeGenContext& ctx) const
{
    if (expr) return expr->codeGen(ctx);
    return nullptr;
}

// ===== 函数调用表达式节点 =====
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
        int idx = 0;
        for (const auto& argExpr: args->args) {
            llvm::Value* argVal = argExpr->codeGen(ctx);
            if (!argVal) return nullptr;

            if (idx < static_cast<int>(funcSymbol->paramTypes.size())) {
                const TypeInfo& expected = funcSymbol->paramTypes[idx];
                llvm::Type* expectedType = expected.kind == SymbolKind::Array
                    ? typeInfoToLLVMType(expected, ctx.context, true)
                    : typeInfoToLLVMValueType(expected, ctx.context);
                argVal = castValueToType(argVal, expectedType, ctx);
            }
            argsV.push_back(argVal);
            idx++;
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
