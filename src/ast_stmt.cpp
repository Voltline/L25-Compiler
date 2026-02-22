#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/DataLayout.h>
#include <cassert>

// ===== 声明语句节点 =====
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
    const std::string& ident_name = name->ident;
    auto typeInfo = name->type;

    // 自动推导：如果类型为默认 Int 且 RHS 是字符串字面量，则推导为 String
    if (typeInfo.kind == SymbolKind::Int && typeInfo.pointerLevel == 0 && expr) {
        TypeInfo rhsType = evaluateExprType(expr.get());
        if (rhsType.kind == SymbolKind::String) {
            typeInfo = TypeInfo{ SymbolKind::String, {}, 0, false };
            // 更新符号表中的类型
            SymbolInfo* symbolInfo = scope->lookupLocal(ident_name);
            if (symbolInfo) {
                symbolInfo->kind = SymbolKind::String;
            }
        }
    }

    llvm::Type* valueType = typeInfoToLLVMValueType(typeInfo, ctx.context);
    llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(valueType, nullptr, ident_name);

    if ((typeInfo.kind == SymbolKind::Int || typeInfo.kind == SymbolKind::Float) && typeInfo.pointerLevel == 0) {
        if (!expr) {
            llvm::Value* zeroInit = typeInfo.kind == SymbolKind::Float
                ? static_cast<llvm::Value*>(llvm::ConstantFP::get(valueType, 0.0))
                : static_cast<llvm::Value*>(llvm::ConstantInt::get(valueType, 0));
            ctx.builder.CreateStore(zeroInit, alloca);
        }
    } else if (typeInfo.kind == SymbolKind::String && typeInfo.pointerLevel == 0) {
        // 字符串零初始化：len=0, data=null
        if (!expr) {
            llvm::StructType* strTy = getL25StringType(ctx.context);
            llvm::Value* zero = llvm::ConstantAggregateZero::get(strTy);
            ctx.builder.CreateStore(zero, alloca);
        }
    } else if (typeInfo.kind == SymbolKind::Array && typeInfo.pointerLevel == 0) {
        // 类型参数列表：i8* 和 i64
        auto memsetFn = llvm::Intrinsic::getDeclaration(
            &ctx.module,
            llvm::Intrinsic::memset,
            {
                llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                llvm::Type::getInt64Ty(ctx.context)
            }
        );

        llvm::Value* zeroVal = llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::Value* sizeVal = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(ctx.context),
            ctx.module.getDataLayout().getTypeAllocSize(valueType)
        );
        llvm::Value* isVolatile = llvm::ConstantInt::getFalse(ctx.context);

        ctx.builder.CreateCall(memsetFn, {
            ctx.builder.CreateBitCast(alloca, llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0)),
            zeroVal,
            sizeVal,
            isVolatile
        });
    } else if (valueType->isPointerTy()) {
        // 指针类型无条件初始化为 null（确保 GC 根栈 push 前 slot 已有效）
        ctx.builder.CreateStore(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(valueType)), alloca);
    }

    // Vector/Map 自动创建
    if (typeInfo.kind == SymbolKind::Vector && typeInfo.pointerLevel == 0) {
        ensureContainerRuntimeDeclared(ctx);
        // 计算元素大小
        TypeInfo elemType = typeInfo.typeParams.empty() ? TypeInfo{ SymbolKind::Int, {}, 0 } : typeInfo.typeParams[0];
        uint64_t elemSize = getTypeAllocSize(elemType, ctx);
        llvm::Value* elemSizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), elemSize);
        llvm::FunctionCallee createFn = ctx.module.getOrInsertFunction("l25_vector_create",
            llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                                    {llvm::Type::getInt64Ty(ctx.context)}, false));
        llvm::Value* vecPtr = ctx.builder.CreateCall(createFn, {elemSizeVal}, "vec.create");
        ctx.builder.CreateStore(vecPtr, alloca);
    } else if (typeInfo.kind == SymbolKind::Map && typeInfo.pointerLevel == 0) {
        ensureContainerRuntimeDeclared(ctx);
        TypeInfo keyType = typeInfo.typeParams.size() >= 1 ? typeInfo.typeParams[0] : TypeInfo{ SymbolKind::Int, {}, 0 };
        TypeInfo valType = typeInfo.typeParams.size() >= 2 ? typeInfo.typeParams[1] : TypeInfo{ SymbolKind::Int, {}, 0 };
        uint64_t keySize = getTypeAllocSize(keyType, ctx);
        uint64_t valSize = getTypeAllocSize(valType, ctx);
        int32_t keyTag = getMapKeyTypeTag(keyType);
        llvm::Value* keySizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), keySize);
        llvm::Value* valSizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), valSize);
        llvm::Value* keyTagVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), keyTag);
        llvm::FunctionCallee createFn = ctx.module.getOrInsertFunction("l25_map_create",
            llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                                    {llvm::Type::getInt64Ty(ctx.context), llvm::Type::getInt64Ty(ctx.context),
                                     llvm::Type::getInt32Ty(ctx.context)}, false));
        llvm::Value* mapPtr = ctx.builder.CreateCall(createFn, {keySizeVal, valSizeVal, keyTagVal}, "map.create");
        ctx.builder.CreateStore(mapPtr, alloca);
    }

    if (!alloca) {
        reportError("无法为变量: " + ident_name + " 分配空间");
        return nullptr;
    }

    // 找到符号
    SymbolInfo* symbolInfo = scope->lookupLocal(ident_name);
    symbolInfo->addr = alloca;

    // 注册 RAII 清理
    if (typeInfo.kind == SymbolKind::String && typeInfo.pointerLevel == 0) {
        ctx.registerCleanup(alloca, CleanupKind::String);
    } else if (typeInfo.kind == SymbolKind::Class && typeInfo.pointerLevel > 0) {
        // GC 模式：内联 push 根栈（替代 l25_gc_add_root 函数调用）
        emitInlineRootPush(alloca, ctx);
        ctx.registerCleanup(alloca, CleanupKind::ClassPtr, typeInfo.className);
        symbolInfo->hasCleanup = true;
    } else if (typeInfo.kind == SymbolKind::Pointer && typeInfo.pointerLevel > 0 && typeInfo.className.empty()) {
        // 基本类型指针（new T[n]）：注册 GC 根 + 清理
        emitInlineRootPush(alloca, ctx);
        ctx.registerCleanup(alloca, CleanupKind::GCRoot);
        symbolInfo->hasCleanup = true;
    } else if (typeInfo.kind == SymbolKind::Vector && typeInfo.pointerLevel == 0) {
        ctx.registerCleanup(alloca, CleanupKind::Vector);
        symbolInfo->hasCleanup = true;
    } else if (typeInfo.kind == SymbolKind::Map && typeInfo.pointerLevel == 0) {
        ctx.registerCleanup(alloca, CleanupKind::Map);
        symbolInfo->hasCleanup = true;
    }

    // 存在赋值
    if (expr) {
        llvm::Value* initVal = expr->codeGen(ctx);
        if (initVal) {
            // 字符串深拷贝：确保变量拥有独立的 malloc 缓冲区
            // 但若 RHS 已产生拥有所有权的缓冲区（拼接/函数返回），跳过深拷贝
            if (typeInfo.kind == SymbolKind::String && typeInfo.pointerLevel == 0) {
                if (!isOwnedStringExpr(expr.get())) {
                    initVal = emitStringDeepCopy(initVal, ctx);
                }
            }
            llvm::Type* targetType = valueType;
            llvm::Value* stored = castValueToType(initVal, targetType, ctx);
            ctx.builder.CreateStore(stored, alloca);
        }
    }
    return alloca;
}

// ===== 赋值语句节点 =====
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
    
    SymbolInfo* targetSymbol = nullptr;

    if (auto identExpr = dynamic_cast<IdentExpr*>(name.get())) {
        SymbolInfo* symbol = scope->lookup(identExpr->ident);
        if (!symbol || !symbol->addr) {
            reportError("变量未声明或未分配空间: " + identExpr->ident);
            return nullptr;
        }
        lhsAddr = symbol->addr;
        targetSymbol = symbol;
    } else if (auto arrayExpr = dynamic_cast<ArraySubscriptExpr*>(name.get())) {
        lhsAddr = arrayExpr->getAddress(ctx);
        if (arrayExpr->array && arrayExpr->array->scope) {
            targetSymbol = arrayExpr->array->scope->lookup(arrayExpr->array->ident);
        }
        if (!lhsAddr) {
            reportError("获取数组元素地址失败");
            return nullptr;
        }
    } else if (auto memberExpr = dynamic_cast<MemberAccessExpr*>(name.get())) {
        lhsAddr = memberExpr->getPointer(ctx);
        if (!lhsAddr) {
            reportError("获取成员地址失败");
            return nullptr;
        }
    } else if (auto derefExpr = dynamic_cast<DereferenceExpr*>(name.get())) {
        lhsAddr = derefExpr->getPointerValue(ctx);
        if (!lhsAddr || !lhsAddr->getType()->isPointerTy()) {
            reportError("解引用目标不是合法的指针地址");
            return nullptr;
        }
    } else {
        reportError("左值类型错误");
        return nullptr;
    }

    TypeInfo lhsType = targetSymbol ? typeInfoFromSymbol(targetSymbol) : evaluateExprType(name.get());

    // 指针下标赋值：lhsType 应为元素类型而非指针类型
    if (auto arrayExpr = dynamic_cast<ArraySubscriptExpr*>(name.get())) {
        if (targetSymbol && targetSymbol->kind == SymbolKind::Pointer && targetSymbol->pointerLevel > 0) {
            lhsType = evaluateExprType(name.get());
        }
    }

    // 字符串赋值：释放旧数据 + 深拷贝新值（已拥有的缓冲区跳过深拷贝）
    if (lhsType.kind == SymbolKind::String && lhsType.pointerLevel == 0 && lhsAddr) {
        emitStringFree(lhsAddr, ctx);
        llvm::Value* newVal = rhs;
        if (!isOwnedStringExpr(expr.get())) {
            newVal = emitStringDeepCopy(rhs, ctx);
        }
        ctx.builder.CreateStore(newVal, lhsAddr);
        return newVal;
    }

    // 类指针赋值
    if (lhsType.kind == SymbolKind::Class && lhsType.pointerLevel > 0 && lhsAddr && !lhsType.className.empty()) {
        // GC 模式：无需释放旧值，GC 负责回收不可达对象
        // 也无需移动语义——多个变量可安全指向同一对象

        // 写屏障：当向堆对象的字段中写入指针时，需通知 GC
        // 仅对 MemberAccessExpr（obj.field = x）和 DereferenceExpr（*p = x）触发
        // 局部变量赋值不需要，因为局部变量已作为根被追踪
        bool needWriteBarrier = dynamic_cast<MemberAccessExpr*>(name.get()) != nullptr
                             || dynamic_cast<DereferenceExpr*>(name.get()) != nullptr;
        if (needWriteBarrier) {
            ensureGCRuntimeDeclared(ctx);
            auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
            llvm::Value* rhsCast = ctx.builder.CreateBitCast(rhs, i8PtrTy, "gc.wb.ptr");
            ctx.builder.CreateCall(ctx.module.getFunction("l25_gc_write_barrier"), {rhsCast});
        }
    }

    llvm::Type* targetType = typeInfoToLLVMValueType(lhsType, ctx.context);
    llvm::Value* storedValue = castValueToType(rhs, targetType, ctx);
    ctx.builder.CreateStore(storedValue, lhsAddr);

    // GC 模式下无需自动移动语义，多个引用可安全共存

    return rhs;
}

// ===== 条件分支语句节点 =====
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
    ctx.pushCleanupScope();
    if_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        emitScopeCleanup(ctx);
        ctx.builder.CreateBr(merge);
    } else {
        ctx.popCleanupScope();
    }

    // else-body
    if (elseBody) {
        ctx.builder.SetInsertPoint(elseBody);
        ctx.currentBlock = elseBody;
        ctx.pushCleanupScope();
        else_body->codeGen(ctx);

        if (!ctx.currentBlock->getTerminator()) {
            ctx.builder.SetInsertPoint(ctx.currentBlock);
            emitScopeCleanup(ctx);
            ctx.builder.CreateBr(merge);
        } else {
            ctx.popCleanupScope();
        }
    }

    // 合并块
    ctx.builder.SetInsertPoint(merge);
    ctx.currentBlock = merge;

    return nullptr;
}

// ===== While循环语句节点 =====
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

    ctx.pushCleanupScope();
    loop_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        emitScopeCleanup(ctx);
        ctx.builder.CreateBr(condBlock);
    } else {
        ctx.popCleanupScope();
    }

    // 必须插入 afterBlock，不然后续的代码可能跳不到这里
    ctx.builder.SetInsertPoint(afterBlock);
    ctx.currentBlock = afterBlock;

    return nullptr;
}

// ===== For循环语句节点 =====
ForStmt::ForStmt(std::unique_ptr<Stmt> init, std::unique_ptr<BoolExpr> condition,
                 std::unique_ptr<Stmt> step, std::unique_ptr<StmtList> loop_body)
    : init(std::move(init))
    , condition(std::move(condition))
    , step(std::move(step))
    , loop_body(std::move(loop_body)) {}

void ForStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "for" << std::endl;
    if (init) init->print(indent + 2);
    condition->print(indent + 2);
    if (step) step->print(indent + 2);
    loop_body->print(indent + 2);
}

llvm::Value* ForStmt::codeGen(CodeGenContext& ctx) const
{
    llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

    // 创建基本块
    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(ctx.context, "for.cond", function);
    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(ctx.context, "for.body", function);
    llvm::BasicBlock* stepBlock = llvm::BasicBlock::Create(ctx.context, "for.step", function);
    llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(ctx.context, "for.after", function);

    // 执行初始化语句
    if (init) {
        init->codeGen(ctx);
    }

    // 跳转到 condBlock
    ctx.builder.CreateBr(condBlock);

    // condBlock: 评估循环条件
    ctx.builder.SetInsertPoint(condBlock);
    ctx.currentBlock = condBlock;

    llvm::Value* condValue = condition->codeGen(ctx);
    if (!condValue) return nullptr;

    if (!condValue->getType()->isIntegerTy(1)) {
        condValue = ctx.builder.CreateICmpNE(condValue, llvm::ConstantInt::get(condValue->getType(), 0), "forcond");
    }

    ctx.builder.CreateCondBr(condValue, bodyBlock, afterBlock);

    // bodyBlock: 循环体
    ctx.builder.SetInsertPoint(bodyBlock);
    ctx.currentBlock = bodyBlock;

    ctx.pushCleanupScope();
    loop_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        emitScopeCleanup(ctx);
        ctx.builder.CreateBr(stepBlock);
    } else {
        ctx.popCleanupScope();
    }

    // stepBlock: 步进语句
    ctx.builder.SetInsertPoint(stepBlock);
    ctx.currentBlock = stepBlock;

    if (step) {
        step->codeGen(ctx);
    }

    ctx.builder.CreateBr(condBlock);

    // afterBlock
    ctx.builder.SetInsertPoint(afterBlock);
    ctx.currentBlock = afterBlock;

    return nullptr;
}

// ===== 输入语句节点 =====
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

    for (auto& ident: idents) {
        assert(scope && "InputStmt::codeGen 中的 scope 为空");
        llvm::Value* addr = nullptr;
        bool expectFloat = false;
        bool expectString = false;
        if (auto* idExpr = dynamic_cast<IdentExpr*>(ident.get())) {
            SymbolInfo* symbol = scope->lookup(idExpr->ident);
            if (!symbol) {
                reportError("变量: " + idExpr->ident + " 未声明");
                return nullptr;
            }

            expectFloat = symbol->isFloat;
            expectString = (symbol->kind == SymbolKind::String);

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
            SymbolInfo* arraySymbol = scope->lookup(arraySubscriptExpr->array->ident);
            if (arraySymbol) {
                expectFloat = arraySymbol->isFloat;
            }
            if (!addr) {
                reportError("数组下标访问异常");
            }
        }

        if (expectString) {
            // 字符串输入：释放旧缓冲区，再分配新缓冲区
            ensureStringRuntimeDeclared(ctx);
            auto* i64Ty = llvm::Type::getInt64Ty(ctx.context);
            auto* i32Ty = llvm::Type::getInt32Ty(ctx.context);

            // 先释放旧 string 数据
            emitStringFree(addr, ctx);

            // malloc(1024) 作为临时缓冲区
            llvm::Value* bufSize = llvm::ConstantInt::get(i64Ty, 1024);
            llvm::Value* buf = ctx.builder.CreateCall(
                ctx.module.getFunction("malloc"), { bufSize }, "input_buf");

            // scanf("%1023s", buf)
            llvm::Value* fmtStr = ctx.builder.CreateGlobalString("%1023s");
            ctx.builder.CreateCall(scanfFunc, { fmtStr, buf });

            // len = strlen(buf)
            llvm::Value* lenI64 = ctx.builder.CreateCall(
                ctx.module.getFunction("strlen"), { buf }, "input_len64");
            llvm::Value* lenI32 = ctx.builder.CreateTrunc(lenI64, i32Ty, "input_len");

            // 构建 __l25_string 并存储
            llvm::StructType* strTy = getL25StringType(ctx.context);
            llvm::Value* strVal = llvm::UndefValue::get(strTy);
            strVal = ctx.builder.CreateInsertValue(strVal, lenI32, 0, "str_set_len");
            strVal = ctx.builder.CreateInsertValue(strVal, buf, 1, "str_set_data");
            ctx.builder.CreateStore(strVal, addr);
        } else {
            std::string fmt = expectFloat ? "%f" : "%d";
            llvm::Value* formatStr = ctx.builder.CreateGlobalString(fmt);
            ctx.builder.CreateCall(scanfFunc, { formatStr, addr });
        }
    }
    return nullptr;
}

// ===== 输出语句节点 =====
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
            if (val->getType()->isIntegerTy(1)) {
                val = ctx.builder.CreateZExt(val, llvm::Type::getInt32Ty(ctx.context));
            }

            // 检查是否为字符串类型 (__l25_string struct)
            llvm::StructType* strTy = getL25StringType(ctx.context);
            if (val->getType() == strTy) {
                formatStr += "%s";
                // 从结构体中提取 data 指针 (index 1)
                llvm::Value* dataPtr = ctx.builder.CreateExtractValue(val, 1, "str_data");
                printfArgs.push_back(dataPtr);
            } else if (val->getType()->isFloatingPointTy()) {
                formatStr += "%f";
                llvm::Value* promoted = ctx.builder.CreateFPExt(val, llvm::Type::getDoubleTy(ctx.context), "fpext_print");
                printfArgs.push_back(promoted);
            } else {
                formatStr += "%d";
                if (val->getType()->isIntegerTy(32)) {
                    printfArgs.push_back(val);
                } else {
                    llvm::Value* casted = castValueToType(val, llvm::Type::getInt32Ty(ctx.context), ctx);
                    printfArgs.push_back(casted);
                }
            }
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

// ===== delete 语句 =====
DeleteStmt::DeleteStmt(std::unique_ptr<Expr> target)
    : target(std::move(target)) {}

void DeleteStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "DeleteStmt" << std::endl;
    if (target) target->print(indent + 2);
}

llvm::Value* DeleteStmt::codeGen(CodeGenContext& ctx) const
{
    if (!target) return nullptr;
    llvm::Value* rawTarget = target->codeGen(ctx);
    if (!rawTarget) return nullptr;

    TypeInfo type = evaluateExprType(target.get());

    // ===== 非类指针（new T[n] 分配的堆数组，GC 管理）=====
    if (type.kind == SymbolKind::Pointer && type.pointerLevel > 0 && type.className.empty()) {
        llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::PointerType* ptrTy = llvm::dyn_cast<llvm::PointerType>(rawTarget->getType());
        if (!ptrTy) ptrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);

        llvm::Function* parentFunc = ctx.builder.GetInsertBlock()->getParent();
        llvm::BasicBlock* deleteBB = llvm::BasicBlock::Create(ctx.context, "delete.arr.body", parentFunc);
        llvm::BasicBlock* contBB = llvm::BasicBlock::Create(ctx.context, "delete.arr.cont", parentFunc);
        llvm::Value* isNull = ctx.builder.CreateICmpEQ(
            ctx.builder.CreateBitCast(rawTarget, i8PtrTy),
            llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)));
        ctx.builder.CreateCondBr(isNull, contBB, deleteBB);

        ctx.builder.SetInsertPoint(deleteBB);
        // GC 统一管理：使用 l25_gc_free 释放
        ensureGCRuntimeDeclared(ctx);
        llvm::Value* castPtr = ctx.builder.CreateBitCast(rawTarget, i8PtrTy);
        llvm::FunctionCallee gcFreeFn = ctx.module.getFunction("l25_gc_free");
        ctx.builder.CreateCall(gcFreeFn, {castPtr});
        ctx.builder.CreateBr(contBB);

        ctx.builder.SetInsertPoint(contBB);

        // 置空源变量（支持 IdentExpr 和 MemberAccessExpr）
        if (auto identExpr = dynamic_cast<IdentExpr*>(target.get())) {
            SymbolInfo* sym = scope->lookup(identExpr->ident);
            if (sym && sym->addr) {
                ctx.builder.CreateStore(
                    llvm::ConstantPointerNull::get(ptrTy), sym->addr);
            }
        } else if (auto memberExpr = dynamic_cast<MemberAccessExpr*>(target.get())) {
            llvm::Value* fieldPtr = memberExpr->getPointer(ctx);
            if (fieldPtr) {
                ctx.builder.CreateStore(
                    llvm::ConstantPointerNull::get(ptrTy), fieldPtr);
            }
        }
        return nullptr;
    }

    // ===== 类指针（GC 管理）=====
    if (type.kind != SymbolKind::Class || type.pointerLevel <= 0) {
        reportError("delete 目标必须是指针");
        return nullptr;
    }

    llvm::StructType* classTy = classStructTypes[type.className];
    if (!classTy) {
        reportError("找不到类类型：" + type.className);
        return nullptr;
    }

    llvm::PointerType* classPtrTy = llvm::PointerType::get(classTy, 0);
    llvm::Value* typedPtr = rawTarget;
    if (typedPtr->getType() != classPtrTy) {
        typedPtr = ctx.builder.CreateBitCast(typedPtr, classPtrTy, "cls.ptr");
    }

    llvm::Function* parentFunc = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* deleteBB = llvm::BasicBlock::Create(ctx.context, "delete.body", parentFunc);
    llvm::BasicBlock* contBB = llvm::BasicBlock::Create(ctx.context, "delete.cont", parentFunc);
    llvm::Value* isNull = ctx.builder.CreateICmpEQ(typedPtr, llvm::ConstantPointerNull::get(classPtrTy));
    ctx.builder.CreateCondBr(isNull, contBB, deleteBB);

    ctx.builder.SetInsertPoint(deleteBB);
    // GC 模式：使用 l25_gc_free 确定性释放（调用析构 + 从 GC 链表移除 + free）
    ensureGCRuntimeDeclared(ctx);
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::Value* castPtr = ctx.builder.CreateBitCast(typedPtr, i8PtrTy);
    llvm::FunctionCallee gcFreeFn = ctx.module.getFunction("l25_gc_free");
    ctx.builder.CreateCall(gcFreeFn, { castPtr });
    ctx.builder.CreateBr(contBB);

    ctx.builder.SetInsertPoint(contBB);

    // 将源变量置空（避免悬挂指针，支持 IdentExpr 和 MemberAccessExpr）
    if (auto identExpr = dynamic_cast<IdentExpr*>(target.get())) {
        SymbolInfo* sym = scope->lookup(identExpr->ident);
        if (sym && sym->addr) {
            ctx.builder.CreateStore(
                llvm::ConstantPointerNull::get(classPtrTy), sym->addr);
        }
    } else if (auto memberExpr = dynamic_cast<MemberAccessExpr*>(target.get())) {
        llvm::Value* fieldPtr = memberExpr->getPointer(ctx);
        if (fieldPtr) {
            ctx.builder.CreateStore(
                llvm::ConstantPointerNull::get(classPtrTy), fieldPtr);
        }
    }

    return nullptr;
}
