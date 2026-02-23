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
    } else if (typeInfo.kind == SymbolKind::Deque && typeInfo.pointerLevel == 0) {
        ensureContainerRuntimeDeclared(ctx);
        TypeInfo elemType = typeInfo.typeParams.empty() ? TypeInfo{ SymbolKind::Int, {}, 0 } : typeInfo.typeParams[0];
        uint64_t elemSize = getTypeAllocSize(elemType, ctx);
        llvm::Value* elemSizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), elemSize);
        llvm::FunctionCallee createFn = ctx.module.getOrInsertFunction("l25_deque_create",
            llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                                    {llvm::Type::getInt64Ty(ctx.context)}, false));
        llvm::Value* deqPtr = ctx.builder.CreateCall(createFn, {elemSizeVal}, "deque.create");
        ctx.builder.CreateStore(deqPtr, alloca);
    } else if (typeInfo.kind == SymbolKind::Queue && typeInfo.pointerLevel == 0) {
        ensureContainerRuntimeDeclared(ctx);
        TypeInfo elemType = typeInfo.typeParams.empty() ? TypeInfo{ SymbolKind::Int, {}, 0 } : typeInfo.typeParams[0];
        uint64_t elemSize = getTypeAllocSize(elemType, ctx);
        llvm::Value* elemSizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), elemSize);
        llvm::FunctionCallee createFn = ctx.module.getOrInsertFunction("l25_queue_create",
            llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                                    {llvm::Type::getInt64Ty(ctx.context)}, false));
        llvm::Value* quePtr = ctx.builder.CreateCall(createFn, {elemSizeVal}, "queue.create");
        ctx.builder.CreateStore(quePtr, alloca);
    } else if (typeInfo.kind == SymbolKind::Channel && typeInfo.pointerLevel == 0) {
        ensureContainerRuntimeDeclared(ctx);
        TypeInfo elemType = typeInfo.typeParams.empty() ? TypeInfo{ SymbolKind::Int, {}, 0 } : typeInfo.typeParams[0];
        uint64_t elemSize = getTypeAllocSize(elemType, ctx);
        llvm::Value* elemSizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), elemSize);
        int cap = typeInfo.channelCapacity; // 0 = 无缓冲 (同步 rendezvous)
        llvm::Value* capVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), cap);
        llvm::FunctionCallee createFn = ctx.module.getOrInsertFunction("l25_channel_create",
            llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                                    {llvm::Type::getInt64Ty(ctx.context), llvm::Type::getInt64Ty(ctx.context)}, false));
        llvm::Value* chPtr = ctx.builder.CreateCall(createFn, {elemSizeVal, capVal}, "channel.create");
        ctx.builder.CreateStore(chPtr, alloca);
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
    } else if (typeInfo.kind == SymbolKind::Deque && typeInfo.pointerLevel == 0) {
        ctx.registerCleanup(alloca, CleanupKind::Deque);
        symbolInfo->hasCleanup = true;
    } else if (typeInfo.kind == SymbolKind::Queue && typeInfo.pointerLevel == 0) {
        ctx.registerCleanup(alloca, CleanupKind::Queue);
        symbolInfo->hasCleanup = true;
    } else if (typeInfo.kind == SymbolKind::Channel && typeInfo.pointerLevel == 0) {
        ctx.registerCleanup(alloca, CleanupKind::Channel);
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
        // 容器下标赋值：lhsType 应为元素/值类型
        if (targetSymbol && (targetSymbol->kind == SymbolKind::Vector
            || targetSymbol->kind == SymbolKind::Map
            || targetSymbol->kind == SymbolKind::Deque)) {
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

    ctx.breakTargets.push_back(afterBlock);
    ctx.pushCleanupScope();
    loop_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        emitScopeCleanup(ctx);
        ctx.builder.CreateBr(condBlock);
    } else {
        ctx.popCleanupScope();
    }
    ctx.breakTargets.pop_back();

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

    ctx.breakTargets.push_back(afterBlock);
    ctx.pushCleanupScope();
    loop_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        emitScopeCleanup(ctx);
        ctx.builder.CreateBr(stepBlock);
    } else {
        ctx.popCleanupScope();
    }
    ctx.breakTargets.pop_back();

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

// ===== Break语句节点 =====
void BreakStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "break" << std::endl;
}

llvm::Value* BreakStmt::codeGen(CodeGenContext& ctx) const
{
    if (ctx.breakTargets.empty()) {
        reportError("break 语句只能在循环内使用");
        return nullptr;
    }
    llvm::BasicBlock* target = ctx.breakTargets.back();
    // 清理当前循环体作用域的 RAII 资源
    emitScopeCleanup(ctx);
    ctx.builder.CreateBr(target);
    // 创建一个不可达的基本块，防止后续语句插入到已终结的块
    llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* deadBlock = llvm::BasicBlock::Create(ctx.context, "break.dead", func);
    ctx.builder.SetInsertPoint(deadBlock);
    ctx.currentBlock = deadBlock;
    return nullptr;
}

// ===== Return语句节点 (早期 return) =====
ReturnStmt::ReturnStmt(std::unique_ptr<Expr> value)
    : value(std::move(value)) {}

void ReturnStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "return" << std::endl;
    if (value) value->print(indent + 2);
}

llvm::Value* ReturnStmt::codeGen(CodeGenContext& ctx) const
{
    if (!ctx.returnBlock || !ctx.retAlloca) {
        reportError("return 语句只能在函数内使用");
        return nullptr;
    }

    // 计算返回值
    llvm::Value* retVal = nullptr;
    if (value) {
        retVal = value->codeGen(ctx);
        if (!retVal) return nullptr;

        // 类型适配：cast 到返回值 alloca 的类型
        llvm::Type* retTy = ctx.retAlloca->getAllocatedType();
        retVal = castValueToType(retVal, retTy, ctx);
        ctx.builder.CreateStore(retVal, ctx.retAlloca);
    }
    // 如果无返回表达式，retAlloca 保持默认值

    // 用 emitReturnCleanup 清理所有活跃作用域
    emitReturnCleanup(ctx);

    // 跳转到函数统一的 return block
    ctx.builder.CreateBr(ctx.returnBlock);

    // 创建 dead block 防止后续语句插入到已终结的块
    llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* deadBlock = llvm::BasicBlock::Create(ctx.context, "return.dead", func);
    ctx.builder.SetInsertPoint(deadBlock);
    ctx.currentBlock = deadBlock;
    return nullptr;
}

// ===== Channel Recv 双返回值语句 =====
ChannelRecvStmt::ChannelRecvStmt(const std::string& valName, const std::string& okName,
                                 std::unique_ptr<Expr> channel)
    : valName(valName), okName(okName), channel(std::move(channel)) {}

void ChannelRecvStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "ChannelRecv: " << valName << ", " << okName << std::endl;
    channel->print(indent + 2);
}

llvm::Value* ChannelRecvStmt::codeGen(CodeGenContext& ctx) const
{
    ensureContainerRuntimeDeclared(ctx);
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::Type* i32Ty   = llvm::Type::getInt32Ty(ctx.context);

    // 获取 channel 指针
    llvm::Value* chPtr = channel->codeGen(ctx);

    // 确定元素类型
    TypeInfo elemType = channelTypeInfo.typeParams.empty()
                          ? TypeInfo{ SymbolKind::Int, {}, 0, false }
                          : channelTypeInfo.typeParams[0];
    llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);

    // 分配输出缓冲
    llvm::AllocaInst* out = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "ch.recv.out");
    llvm::Value* outCast = ctx.builder.CreateBitCast(out, i8PtrTy);

    // 调用 l25_channel_recv_ok
    llvm::FunctionCallee recvOkFn = ctx.module.getFunction("l25_channel_recv_ok");
    llvm::Value* okVal = ctx.builder.CreateCall(recvOkFn, {chPtr, outCast}, "ch.recv.ok");

    // 获取值
    llvm::Value* valVal = ctx.builder.CreateLoad(elemLLVMTy, out, "ch.recv.val");

    // 声明并初始化 val 变量
    SymbolInfo* valSym = scope->lookupLocal(valName);
    llvm::AllocaInst* valAlloca = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, valName);
    ctx.builder.CreateStore(valVal, valAlloca);
    if (valSym) valSym->addr = valAlloca;

    // 注册 GC 根（如果是指针类型）
    if (elemType.pointerLevel > 0 || elemType.kind == SymbolKind::Class) {
        auto* gcPushFn = ctx.module.getFunction("l25_gc_root_push");
        if (gcPushFn) {
            llvm::Value* slotCast = ctx.builder.CreateBitCast(valAlloca,
                llvm::PointerType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0), 0));
            ctx.builder.CreateCall(gcPushFn, {slotCast});
            ctx.registerCleanup(valAlloca, CleanupKind::GCRoot);
        }
    }

    // 声明并初始化 ok 变量
    SymbolInfo* okSym = scope->lookupLocal(okName);
    llvm::AllocaInst* okAlloca = ctx.builder.CreateAlloca(i32Ty, nullptr, okName);
    ctx.builder.CreateStore(okVal, okAlloca);
    if (okSym) okSym->addr = okAlloca;

    return nullptr;
}

// ===== ForRangeChannel 语句 =====
ForRangeChannelStmt::ForRangeChannelStmt(const std::string& valName,
                                         std::unique_ptr<Expr> channel,
                                         std::unique_ptr<StmtList> body)
    : valName(valName), channel(std::move(channel)), body(std::move(body)) {}

void ForRangeChannelStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "ForRangeChannel: " << valName << std::endl;
    channel->print(indent + 2);
    body->print(indent + 2);
}

llvm::Value* ForRangeChannelStmt::codeGen(CodeGenContext& ctx) const
{
    ensureContainerRuntimeDeclared(ctx);
    llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::Type* i32Ty   = llvm::Type::getInt32Ty(ctx.context);

    // 获取 channel 指针
    llvm::Value* chPtr = channel->codeGen(ctx);

    // 确定元素类型
    TypeInfo elemType = channelTypeInfo.typeParams.empty()
                          ? TypeInfo{ SymbolKind::Int, {}, 0, false }
                          : channelTypeInfo.typeParams[0];
    llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);

    // 为循环变量分配栈空间（在循环外，便于循环体内使用）
    llvm::AllocaInst* valAlloca = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, valName);
    llvm::AllocaInst* recvOut   = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "ch.range.out");

    // 获取 loopBodyScope 并设置 val 变量的 addr
    SymbolInfo* valSym = loopBodyScope->lookupLocal(valName);
    if (valSym) valSym->addr = valAlloca;

    // 注册 GC 根（如果是指针类型）
    if (elemType.pointerLevel > 0 || elemType.kind == SymbolKind::Class) {
        auto* gcPushFn = ctx.module.getFunction("l25_gc_root_push");
        if (gcPushFn) {
            llvm::Value* slotCast = ctx.builder.CreateBitCast(valAlloca,
                llvm::PointerType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0), 0));
            ctx.builder.CreateCall(gcPushFn, {slotCast});
            ctx.registerCleanup(valAlloca, CleanupKind::GCRoot);
        }
    }

    // 创建基本块
    llvm::BasicBlock* condBlock  = llvm::BasicBlock::Create(ctx.context, "forch.cond", function);
    llvm::BasicBlock* bodyBlock  = llvm::BasicBlock::Create(ctx.context, "forch.body", function);
    llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(ctx.context, "forch.after", function);

    ctx.builder.CreateBr(condBlock);

    // condBlock: recv_ok → if !ok, break
    ctx.builder.SetInsertPoint(condBlock);
    ctx.currentBlock = condBlock;

    llvm::Value* outCast = ctx.builder.CreateBitCast(recvOut, i8PtrTy);
    llvm::FunctionCallee recvOkFn = ctx.module.getFunction("l25_channel_recv_ok");
    llvm::Value* okVal = ctx.builder.CreateCall(recvOkFn, {chPtr, outCast}, "ch.range.ok");
    llvm::Value* okBool = ctx.builder.CreateICmpNE(okVal, llvm::ConstantInt::get(i32Ty, 0), "ch.range.okbool");

    // 将接收到的值存入循环变量
    llvm::Value* recvVal = ctx.builder.CreateLoad(elemLLVMTy, recvOut, "ch.range.val");
    ctx.builder.CreateStore(recvVal, valAlloca);

    ctx.builder.CreateCondBr(okBool, bodyBlock, afterBlock);

    // bodyBlock
    ctx.builder.SetInsertPoint(bodyBlock);
    ctx.currentBlock = bodyBlock;

    ctx.breakTargets.push_back(afterBlock);
    ctx.pushCleanupScope();
    body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        emitScopeCleanup(ctx);
        ctx.builder.CreateBr(condBlock);
    } else {
        ctx.popCleanupScope();
    }
    ctx.breakTargets.pop_back();

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

// ===== printf 语句（C 风格格式化输出） =====
PrintfStmt::PrintfStmt(std::unique_ptr<ArgList> args)
    : idents(std::move(args->args)) {}

void PrintfStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Printf" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* PrintfStmt::codeGen(CodeGenContext& ctx) const {
    // 获取或声明 printf 函数
    llvm::Function* printfFunc = ctx.module.getFunction("printf");
    if (!printfFunc) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true
        );
        printfFunc = llvm::Function::Create(
            printfType, llvm::Function::ExternalLinkage, "printf", ctx.module);
    }

    if (idents.empty()) return nullptr;

    // 第一个参数必须是格式字符串
    std::vector<llvm::Value*> printfArgs;

    // 评估第一个参数（格式串）
    llvm::Value* fmtVal = idents[0]->codeGen(ctx);
    if (!fmtVal) return nullptr;

    // 如果是 __l25_string 结构体，提取 data 指针
    llvm::StructType* strTy = getL25StringType(ctx.context);
    if (fmtVal->getType() == strTy) {
        fmtVal = ctx.builder.CreateExtractValue(fmtVal, 1, "fmt_data");
    }
    printfArgs.push_back(fmtVal);

    // 后续参数：直接传递，根据类型做必要的类型提升
    for (size_t i = 1; i < idents.size(); i++) {
        llvm::Value* val = idents[i]->codeGen(ctx);
        if (!val) continue;

        if (val->getType()->isIntegerTy(1)) {
            val = ctx.builder.CreateZExt(val, llvm::Type::getInt32Ty(ctx.context));
        }

        // L25 字符串 → 提取 data 指针
        if (val->getType() == strTy) {
            val = ctx.builder.CreateExtractValue(val, 1, "arg_str_data");
        }
        // float → double（C 的可变参数规则: float 提升为 double）
        if (val->getType()->isFloatTy()) {
            val = ctx.builder.CreateFPExt(val, llvm::Type::getDoubleTy(ctx.context), "fpext_printf");
        }

        printfArgs.push_back(val);
    }

    ctx.builder.CreateCall(printfFunc, printfArgs);
    return nullptr;
}

// ===== scanf 语句（C 风格格式化输入） =====
ScanfStmt::ScanfStmt(std::unique_ptr<ArgList> args)
    : idents(std::move(args->args)) {}

void ScanfStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Scanf" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* ScanfStmt::codeGen(CodeGenContext& ctx) const {
    // 获取或声明 scanf 函数
    llvm::Function* scanfFunc = ctx.module.getFunction("scanf");
    if (!scanfFunc) {
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true
        );
        scanfFunc = llvm::Function::Create(
            scanfType, llvm::Function::ExternalLinkage, "scanf", ctx.module);
    }

    if (idents.empty()) return nullptr;

    // 第一个参数是格式字符串
    std::vector<llvm::Value*> scanfArgs;

    llvm::Value* fmtVal = idents[0]->codeGen(ctx);
    if (!fmtVal) return nullptr;

    // 如果是 __l25_string 结构体，提取 data 指针
    llvm::StructType* strTy = getL25StringType(ctx.context);
    if (fmtVal->getType() == strTy) {
        fmtVal = ctx.builder.CreateExtractValue(fmtVal, 1, "scanf_fmt_data");
    }
    scanfArgs.push_back(fmtVal);

    // 后续参数：必须是可取地址的（变量标识符或数组下标）
    for (size_t i = 1; i < idents.size(); i++) {
        llvm::Value* addr = nullptr;

        if (auto* idExpr = dynamic_cast<IdentExpr*>(idents[i].get())) {
            SymbolInfo* symbol = scope->lookup(idExpr->ident);
            if (!symbol) {
                reportError("变量: " + idExpr->ident + " 未声明");
                return nullptr;
            }
            addr = symbol->addr;
        } else if (auto* arraySubscriptExpr = dynamic_cast<ArraySubscriptExpr*>(idents[i].get())) {
            addr = arraySubscriptExpr->getAddress(ctx);
        } else {
            // 不是变量或数组下标，无法取地址
            reportError("scanf 参数必须是变量或数组元素");
            return nullptr;
        }

        if (!addr) {
            reportError("scanf 参数无法获取地址");
            return nullptr;
        }
        scanfArgs.push_back(addr);
    }

    ctx.builder.CreateCall(scanfFunc, scanfArgs);
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

// ===== Select 语句 =====
SelectStmt::SelectStmt(std::vector<std::unique_ptr<SelectCase>> cases)
    : cases(std::move(cases)) {}

void SelectStmt::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Select" << std::endl;
    for (auto& c : cases) {
        std::string kindStr;
        switch (c->kind) {
            case SelectCaseKind::Recv: kindStr = "Recv(" + c->recvVarName + ")"; break;
            case SelectCaseKind::Send: kindStr = "Send"; break;
            case SelectCaseKind::Default: kindStr = "Default"; break;
        }
        std::cout << std::string(indent + 2, ' ') << "Case: " << kindStr << std::endl;
        if (c->channel) c->channel->print(indent + 4);
        if (c->sendValue) c->sendValue->print(indent + 4);
        if (c->body) c->body->print(indent + 4);
    }
}

llvm::Value* SelectStmt::codeGen(CodeGenContext& ctx) const
{
    ensureContainerRuntimeDeclared(ctx);

    llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::Type* i32Ty   = llvm::Type::getInt32Ty(ctx.context);

    // 寻找 default 分支
    int defaultIdx = -1;
    for (int i = 0; i < (int)cases.size(); ++i) {
        if (cases[i]->kind == SelectCaseKind::Default) {
            defaultIdx = i;
            break;
        }
    }

    // 声明 sched_yield (for spin loop without default)
    if (!ctx.module.getFunction("sched_yield")) {
        ctx.module.getOrInsertFunction("sched_yield",
            llvm::FunctionType::get(i32Ty, {}, false));
    }
    llvm::Function* schedYieldFn = ctx.module.getFunction("sched_yield");

    // 创建基本块
    llvm::BasicBlock* loopBlock  = llvm::BasicBlock::Create(ctx.context, "select.loop", function);
    llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(ctx.context, "select.after", function);

    // 为每个 non-default case 创建 try 和 body 块
    struct CaseBlocks {
        llvm::BasicBlock* tryBlock;
        llvm::BasicBlock* bodyBlock;
    };
    std::vector<CaseBlocks> caseBlocks;
    for (int i = 0; i < (int)cases.size(); ++i) {
        if (cases[i]->kind == SelectCaseKind::Default) {
            caseBlocks.push_back({nullptr, nullptr});
            continue;
        }
        auto* tryBB  = llvm::BasicBlock::Create(ctx.context, "select.try." + std::to_string(i), function);
        auto* bodyBB = llvm::BasicBlock::Create(ctx.context, "select.body." + std::to_string(i), function);
        caseBlocks.push_back({tryBB, bodyBB});
    }

    // default body block
    llvm::BasicBlock* defaultBodyBlock = nullptr;
    if (defaultIdx >= 0) {
        defaultBodyBlock = llvm::BasicBlock::Create(ctx.context, "select.default", function);
    }

    // 跳转到 loopBlock
    ctx.builder.CreateBr(loopBlock);
    ctx.builder.SetInsertPoint(loopBlock);
    ctx.currentBlock = loopBlock;

    // 找到第一个 non-default case
    int firstNonDefault = -1;
    for (int i = 0; i < (int)cases.size(); ++i) {
        if (cases[i]->kind != SelectCaseKind::Default) {
            firstNonDefault = i;
            break;
        }
    }

    if (firstNonDefault >= 0) {
        ctx.builder.CreateBr(caseBlocks[firstNonDefault].tryBlock);
    } else {
        // 只有 default
        ctx.builder.CreateBr(defaultBodyBlock);
    }

    // 生成每个 non-default case 的 try 块
    for (int i = 0; i < (int)cases.size(); ++i) {
        if (cases[i]->kind == SelectCaseKind::Default) continue;

        ctx.builder.SetInsertPoint(caseBlocks[i].tryBlock);
        ctx.currentBlock = caseBlocks[i].tryBlock;

        llvm::Value* chPtr = cases[i]->channel->codeGen(ctx);

        TypeInfo elemType = cases[i]->channelTypeInfo.typeParams.empty()
                              ? TypeInfo{ SymbolKind::Int, {}, 0, false }
                              : cases[i]->channelTypeInfo.typeParams[0];
        llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);

        // 找到下一个要尝试的块
        llvm::BasicBlock* nextTry = nullptr;
        for (int j = i + 1; j < (int)cases.size(); ++j) {
            if (cases[j]->kind != SelectCaseKind::Default) {
                nextTry = caseBlocks[j].tryBlock;
                break;
            }
        }
        // 如果没有下一个 non-default case，回退到 default 或 yield+loop
        llvm::BasicBlock* failBlock = nullptr;
        if (nextTry) {
            failBlock = nextTry;
        } else if (defaultBodyBlock) {
            failBlock = defaultBodyBlock;
        } else {
            // 需要 yield block 然后回到 loopBlock
            failBlock = llvm::BasicBlock::Create(ctx.context, "select.yield", function);
        }

        if (cases[i]->kind == SelectCaseKind::Recv) {
            llvm::AllocaInst* out = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "sel.recv.out");
            llvm::Value* outCast = ctx.builder.CreateBitCast(out, i8PtrTy);
            llvm::Function* tryRecvFn = ctx.module.getFunction("l25_channel_try_recv");
            llvm::Value* okVal = ctx.builder.CreateCall(tryRecvFn, {chPtr, outCast}, "sel.try.recv");
            llvm::Value* okBool = ctx.builder.CreateICmpNE(okVal, llvm::ConstantInt::get(i32Ty, 0));
            ctx.builder.CreateCondBr(okBool, caseBlocks[i].bodyBlock, failBlock);

            // body 块
            ctx.builder.SetInsertPoint(caseBlocks[i].bodyBlock);
            ctx.currentBlock = caseBlocks[i].bodyBlock;

            llvm::Value* recvVal = ctx.builder.CreateLoad(elemLLVMTy, out, "sel.recv.val");
            SymbolInfo* valSym = cases[i]->bodyScope ? cases[i]->bodyScope->lookupLocal(cases[i]->recvVarName) : nullptr;
            llvm::AllocaInst* valAlloca = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, cases[i]->recvVarName);
            ctx.builder.CreateStore(recvVal, valAlloca);
            if (valSym) valSym->addr = valAlloca;

            if (elemType.pointerLevel > 0 || elemType.kind == SymbolKind::Class) {
                auto* gcPushFn = ctx.module.getFunction("l25_gc_root_push");
                if (gcPushFn) {
                    llvm::Value* slotCast = ctx.builder.CreateBitCast(valAlloca,
                        llvm::PointerType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0), 0));
                    ctx.builder.CreateCall(gcPushFn, {slotCast});
                    ctx.registerCleanup(valAlloca, CleanupKind::GCRoot);
                }
            }

            ctx.pushCleanupScope();
            cases[i]->body->codeGen(ctx);
            if (!ctx.currentBlock->getTerminator()) {
                emitScopeCleanup(ctx);
                ctx.builder.CreateBr(afterBlock);
            } else {
                ctx.popCleanupScope();
            }

        } else if (cases[i]->kind == SelectCaseKind::Send) {
            llvm::Value* sendVal = cases[i]->sendValue->codeGen(ctx);
            llvm::AllocaInst* sendBuf = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "sel.send.buf");
            ctx.builder.CreateStore(sendVal, sendBuf);
            llvm::Value* sendCast = ctx.builder.CreateBitCast(sendBuf, i8PtrTy);
            llvm::Function* trySendFn = ctx.module.getFunction("l25_channel_try_send");
            llvm::Value* okVal = ctx.builder.CreateCall(trySendFn, {chPtr, sendCast}, "sel.try.send");
            llvm::Value* okBool = ctx.builder.CreateICmpNE(okVal, llvm::ConstantInt::get(i32Ty, 0));
            ctx.builder.CreateCondBr(okBool, caseBlocks[i].bodyBlock, failBlock);

            // body 块
            ctx.builder.SetInsertPoint(caseBlocks[i].bodyBlock);
            ctx.currentBlock = caseBlocks[i].bodyBlock;

            ctx.pushCleanupScope();
            cases[i]->body->codeGen(ctx);
            if (!ctx.currentBlock->getTerminator()) {
                emitScopeCleanup(ctx);
                ctx.builder.CreateBr(afterBlock);
            } else {
                ctx.popCleanupScope();
            }
        }

        // 如果 failBlock 是一个 yield block（无 default、无 nextTry），填充它
        if (!nextTry && !defaultBodyBlock) {
            ctx.builder.SetInsertPoint(failBlock);
            ctx.currentBlock = failBlock;
            ctx.builder.CreateCall(schedYieldFn);
            ctx.builder.CreateBr(loopBlock);
        }
    }

    // 生成 default body 块
    if (defaultIdx >= 0 && defaultBodyBlock) {
        ctx.builder.SetInsertPoint(defaultBodyBlock);
        ctx.currentBlock = defaultBodyBlock;

        ctx.pushCleanupScope();
        cases[defaultIdx]->body->codeGen(ctx);
        if (!ctx.currentBlock->getTerminator()) {
            emitScopeCleanup(ctx);
            ctx.builder.CreateBr(afterBlock);
        } else {
            ctx.popCleanupScope();
        }
    }

    // afterBlock
    ctx.builder.SetInsertPoint(afterBlock);
    ctx.currentBlock = afterBlock;

    return nullptr;
}
