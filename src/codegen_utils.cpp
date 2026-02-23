#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/DataLayout.h>
#include <unordered_map>
#include <algorithm>

// ===== 全局变量定义 =====
std::unordered_map<std::string, int> functionMap;
std::unordered_map<std::string, llvm::StructType*> classStructTypes;
std::unordered_map<std::string, std::vector<std::pair<std::string, TypeInfo>>> classFieldLayouts;
std::unordered_map<std::string, std::unordered_map<std::string, TypeInfo>> classMethodReturnTypes;
std::unordered_map<std::string, std::vector<std::string>> classMethodNames;

// ===== L25 String 结构体类型 { i32 len, i8* data } =====
llvm::StructType* getL25StringType(llvm::LLVMContext& ctx) {
    llvm::StructType* ty = llvm::StructType::getTypeByName(ctx, "__l25_string");
    if (!ty) {
        ty = llvm::StructType::create(ctx, "__l25_string");
        ty->setBody({
            llvm::Type::getInt32Ty(ctx),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0)
        }, false);
    }
    return ty;
}

// 确保 libc 辅助函数已声明
void ensureStringRuntimeDeclared(CodeGenContext& ctx) {
    auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    auto* i32Ty = llvm::Type::getInt32Ty(ctx.context);
    auto* i64Ty = llvm::Type::getInt64Ty(ctx.context);

    if (!ctx.module.getFunction("malloc")) {
        ctx.module.getOrInsertFunction("malloc",
            llvm::FunctionType::get(i8PtrTy, { i64Ty }, false));
    }
    if (!ctx.module.getFunction("free")) {
        ctx.module.getOrInsertFunction("free",
            llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), { i8PtrTy }, false));
    }
    if (!ctx.module.getFunction("strlen")) {
        ctx.module.getOrInsertFunction("strlen",
            llvm::FunctionType::get(i64Ty, { i8PtrTy }, false));
    }
    if (!ctx.module.getFunction("strcmp")) {
        ctx.module.getOrInsertFunction("strcmp",
            llvm::FunctionType::get(i32Ty, { i8PtrTy, i8PtrTy }, false));
    }
    if (!ctx.module.getFunction("memcpy")) {
        ctx.module.getOrInsertFunction("memcpy",
            llvm::FunctionType::get(i8PtrTy, { i8PtrTy, i8PtrTy, i64Ty }, false));
    }
    if (!ctx.module.getFunction("snprintf")) {
        ctx.module.getOrInsertFunction("snprintf",
            llvm::FunctionType::get(i32Ty, { i8PtrTy, i64Ty, i8PtrTy }, true));
    }
}

// ===== 内部工具函数 =====
static llvm::Type* wrapPointer(llvm::Type* base, int pointerLevel)
{
    for (int i = 0; i < pointerLevel; ++i) {
        base = llvm::PointerType::get(base, 0);
    }
    return base;
}

llvm::Type* buildArrayType(llvm::Type* elementType, const std::vector<int>& dims)
{
    llvm::Type* arrayType = elementType;
    for (auto it = dims.rbegin(); it != dims.rend(); ++it) {
        arrayType = llvm::ArrayType::get(arrayType, *it);
    }
    return arrayType;
}

llvm::Type* typeInfoToLLVMType(const TypeInfo& typeInfo, llvm::LLVMContext& ctx, bool decayArrayToPointer)
{
    llvm::Type* baseType = nullptr;
    llvm::Type* scalarType = (typeInfo.kind == SymbolKind::Float || typeInfo.isFloat)
        ? llvm::Type::getFloatTy(ctx)
        : llvm::Type::getInt32Ty(ctx);

    if (typeInfo.kind == SymbolKind::Int || typeInfo.kind == SymbolKind::Float) {
        baseType = scalarType;
    } else if (typeInfo.kind == SymbolKind::Array) {
        auto* arrayType = buildArrayType(scalarType, typeInfo.dims);
        baseType = decayArrayToPointer ? llvm::PointerType::get(arrayType, 0) : arrayType;
    } else if (typeInfo.kind == SymbolKind::Pointer) {
        baseType = scalarType;
    } else if (typeInfo.kind == SymbolKind::Class) {
        auto it = classStructTypes.find(typeInfo.className);
        if (it != classStructTypes.end()) {
            baseType = it->second;
        }
    } else if (typeInfo.kind == SymbolKind::String) {
        baseType = getL25StringType(ctx);
    } else if (typeInfo.kind == SymbolKind::Vector || typeInfo.kind == SymbolKind::Map
            || typeInfo.kind == SymbolKind::Deque  || typeInfo.kind == SymbolKind::Queue
            || typeInfo.kind == SymbolKind::Channel) {
        // 容器类型在 IR 层是不透明指针 (i8*)
        baseType = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0);
    }

    if (!baseType) return nullptr;

    int wrapTimes = typeInfo.pointerLevel;
    if (typeInfo.kind == SymbolKind::Array && decayArrayToPointer && wrapTimes > 0) {
        // 数组已经退化为指针，额外的指针层级需要在此基础上继续包裹
        wrapTimes -= 1;
    }
    return wrapPointer(baseType, wrapTimes);
}

llvm::Type* typeInfoToLLVMValueType(const TypeInfo& typeInfo, llvm::LLVMContext& ctx)
{
    llvm::Type* scalarType = (typeInfo.kind == SymbolKind::Float || typeInfo.isFloat)
        ? llvm::Type::getFloatTy(ctx)
        : llvm::Type::getInt32Ty(ctx);

    if (typeInfo.kind == SymbolKind::Pointer) {
        return wrapPointer(scalarType, std::max(1, typeInfo.pointerLevel));
    }

    llvm::Type* baseType = nullptr;
    if (typeInfo.kind == SymbolKind::Array) {
        baseType = buildArrayType(scalarType, typeInfo.dims);
    } else if (typeInfo.kind == SymbolKind::Class) {
        auto it = classStructTypes.find(typeInfo.className);
        if (it != classStructTypes.end()) {
            baseType = it->second;
        }
    } else if (typeInfo.kind == SymbolKind::String) {
        baseType = getL25StringType(ctx);
    } else if (typeInfo.kind == SymbolKind::Vector || typeInfo.kind == SymbolKind::Map
            || typeInfo.kind == SymbolKind::Deque  || typeInfo.kind == SymbolKind::Queue
            || typeInfo.kind == SymbolKind::Channel) {
        baseType = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0);
    } else {
        baseType = scalarType;
    }

    if (!baseType) return nullptr;

    return wrapPointer(baseType, typeInfo.pointerLevel);
}

llvm::Value* castValueToType(llvm::Value* value, llvm::Type* targetType, CodeGenContext& ctx)
{
    if (!value || !targetType) return value;
    llvm::Type* srcType = value->getType();
    if (srcType == targetType) return value;

    if (targetType->isFloatTy()) {
        if (srcType->isIntegerTy()) {
            return ctx.builder.CreateSIToFP(value, targetType, "sitofp");
        }
        if (srcType->isFloatTy()) {
            return ctx.builder.CreateFPCast(value, targetType, "fpc");
        }
    }

    if (targetType->isIntegerTy()) {
        if (srcType->isFloatTy()) {
            return ctx.builder.CreateFPToSI(value, targetType, "fptosi");
        }
        if (srcType->isIntegerTy()) {
            return ctx.builder.CreateIntCast(value, targetType, true, "intcast");
        }
        if (srcType->isPointerTy()) {
            unsigned ptrBits = ctx.module.getDataLayout().getPointerSizeInBits();
            llvm::IntegerType* ptrIntTy = llvm::IntegerType::get(ctx.context, ptrBits ? ptrBits : 64);
            llvm::Value* casted = ctx.builder.CreatePtrToInt(value, ptrIntTy, "ptrtoint");
            if (ptrIntTy != targetType) {
                casted = ctx.builder.CreateIntCast(casted, targetType, true, "intcast");
            }
            return casted;
        }
    }

    if (targetType->isPointerTy() && srcType->isPointerTy()) {
        return ctx.builder.CreateBitCast(value, targetType, "bitcast");
    }

    if (targetType->isPointerTy() && srcType->isIntegerTy()) {
        unsigned ptrBits = ctx.module.getDataLayout().getPointerSizeInBits();
        llvm::IntegerType* ptrIntTy = llvm::IntegerType::get(ctx.context, ptrBits ? ptrBits : 64);
        llvm::Value* casted = value;
        if (srcType != ptrIntTy) {
            casted = ctx.builder.CreateIntCast(value, ptrIntTy, true, "intcast");
        }
        return ctx.builder.CreateIntToPtr(casted, targetType, "inttoptr");
    }

    return value;
}

llvm::Value* defaultValueForType(const TypeInfo& typeInfo, CodeGenContext& ctx)
{
    llvm::Type* llvmTy = typeInfoToLLVMValueType(typeInfo, ctx.context);
    if (!llvmTy) {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }

    if (llvmTy->isPointerTy()) {
        return llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(llvmTy));
    }
    if (llvmTy->isFloatingPointTy()) {
        return llvm::ConstantFP::get(llvmTy, 0.0);
    }
    if (llvmTy->isIntegerTy()) {
        return llvm::ConstantInt::get(llvmTy, 0);
    }
    if (llvmTy->isAggregateType()) {
        return llvm::ConstantAggregateZero::get(llvmTy);
    }
    return llvm::UndefValue::get(llvmTy);
}

std::string buildCtorName(const std::string& className, size_t paramCount)
{
    return className + ".__ctor" + std::to_string(paramCount);
}

std::string buildDtorName(const std::string& className)
{
    return className + ".__dtor";
}

TypeInfo typeInfoFromSymbol(const SymbolInfo* symbol)
{
    if (!symbol) return TypeInfo{};
    TypeInfo ti{ symbol->kind, symbol->dimensions, symbol->pointerLevel, symbol->isFloat, symbol->className };
    ti.typeParams = symbol->typeParams;
    return ti;
}

TypeInfo evaluateExprType(const Expr* expr)
{
    if (!expr) return TypeInfo{ SymbolKind::Invalid, {}, 0 };
    if (auto num = dynamic_cast<const NumberExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Int, {}, 0, false, "" };
    }
    if (dynamic_cast<const FloatNumberExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Float, {}, 0, true };
    }
    if (dynamic_cast<const NilExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Pointer, {}, 1, false };
    }
    if (dynamic_cast<const StringLiteralExpr*>(expr)) {
        return TypeInfo{ SymbolKind::String, {}, 0, false };
    }
    if (dynamic_cast<const StrlenExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Int, {}, 0, false };
    }
    if (dynamic_cast<const TypenameExpr*>(expr)) {
        return TypeInfo{ SymbolKind::String, {}, 0, false };
    }
    if (dynamic_cast<const FieldCountExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Int, {}, 0, false };
    }
    if (dynamic_cast<const MethodCountExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Int, {}, 0, false };
    }
    if (dynamic_cast<const FieldNameExpr*>(expr)) {
        return TypeInfo{ SymbolKind::String, {}, 0, false };
    }
    if (dynamic_cast<const MethodNameExpr*>(expr)) {
        return TypeInfo{ SymbolKind::String, {}, 0, false };
    }
    if (dynamic_cast<const InvokeExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Int, {}, 0, false };
    }
    if (auto ident = dynamic_cast<const IdentExpr*>(expr)) {
        SymbolInfo* symbol = ident->scope ? ident->scope->lookup(ident->ident) : nullptr;
        if (symbol) return typeInfoFromSymbol(symbol);
        return TypeInfo{ SymbolKind::Int, {}, 0 };
    }
    if (auto arrayExpr = dynamic_cast<const ArraySubscriptExpr*>(expr)) {
        Scope* lookupScope = arrayExpr->scope ? arrayExpr->scope : (arrayExpr->array ? arrayExpr->array->scope : nullptr);
        SymbolInfo* symbol = lookupScope ? lookupScope->lookup(arrayExpr->array->ident) : nullptr;
        if (symbol) {
            if (symbol->kind == SymbolKind::Vector || symbol->kind == SymbolKind::Deque) {
                return getContainerElemType(symbol);
            }
            if (symbol->kind == SymbolKind::Map) {
                return getContainerValueType(symbol);
            }
        }
        bool isFloatElem = symbol && symbol->isFloat;
        return TypeInfo{ isFloatElem ? SymbolKind::Float : SymbolKind::Int, {}, 0, isFloatElem };
    }
    if (auto addrExpr = dynamic_cast<const AddressOfExpr*>(expr)) {
        TypeInfo baseType = evaluateExprType(addrExpr->target.get());
        if (baseType.kind == SymbolKind::Array) {
            baseType.kind = SymbolKind::Pointer;
            baseType.dims.clear();
        } else if (baseType.kind == SymbolKind::Invalid) {
            baseType.kind = SymbolKind::Pointer;
        }
        baseType.pointerLevel += 1;
        if (baseType.kind == SymbolKind::Int || baseType.kind == SymbolKind::Float) {
            baseType.kind = SymbolKind::Pointer;
        }
        return baseType;
    }
    if (auto derefExpr = dynamic_cast<const DereferenceExpr*>(expr)) {
        TypeInfo baseType = evaluateExprType(derefExpr->pointerExpr.get());
        if (baseType.pointerLevel > 0) {
            baseType.pointerLevel -= 1;
            if (baseType.pointerLevel == 0 && baseType.kind == SymbolKind::Pointer) {
                baseType.kind = baseType.isFloat ? SymbolKind::Float : SymbolKind::Int;
            }
        } else {
            baseType.kind = baseType.isFloat ? SymbolKind::Float : SymbolKind::Int;
        }
        return baseType;
    }
    if (auto binary = dynamic_cast<const BinaryExpr*>(expr)) {
        TypeInfo lhsType = evaluateExprType(binary->lhs.get());
        TypeInfo rhsType = evaluateExprType(binary->rhs.get());
        // 字符串拼接结果为 String
        if (lhsType.kind == SymbolKind::String || rhsType.kind == SymbolKind::String) {
            return TypeInfo{ SymbolKind::String, {}, 0, false };
        }
        bool isFloatResult = lhsType.isFloat || rhsType.isFloat || lhsType.kind == SymbolKind::Float || rhsType.kind == SymbolKind::Float;
        if (isFloatResult) {
            return TypeInfo{ SymbolKind::Float, {}, 0, true };
        }
        return TypeInfo{ SymbolKind::Int, {}, 0 };
    }
    if (auto unary = dynamic_cast<const UnaryExpr*>(expr)) {
        TypeInfo rhsType = evaluateExprType(unary->rhs.get());
        if (rhsType.isFloat || rhsType.kind == SymbolKind::Float) {
            return TypeInfo{ SymbolKind::Float, {}, rhsType.pointerLevel, true };
        }
        return TypeInfo{ SymbolKind::Int, {}, rhsType.pointerLevel };
    }
    if (auto member = dynamic_cast<const MemberAccessExpr*>(expr)) {
        TypeInfo targetType = evaluateExprType(member->target.get());
        auto layoutIt = classFieldLayouts.find(targetType.className);
        if (layoutIt != classFieldLayouts.end()) {
            auto fit = std::find_if(layoutIt->second.begin(), layoutIt->second.end(), [&](const auto& f){return f.first == member->member->ident;});
            if (fit != layoutIt->second.end()) {
                return fit->second;
            }
        }
        return TypeInfo{ SymbolKind::Invalid, {}, 0 };
    }
    if (auto newExpr = dynamic_cast<const NewExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Class, {}, 1, false, newExpr->className->ident };
    }
    if (auto newArrExpr = dynamic_cast<const NewArrayExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Pointer, {}, 1, newArrExpr->isFloat };
    }
    if (auto methodCall = dynamic_cast<const MethodCallExpr*>(expr)) {
        TypeInfo targetType = evaluateExprType(methodCall->target.get());
        // 容器方法返回类型
        if (targetType.kind == SymbolKind::Vector) {
            const std::string& mname = methodCall->method->ident;
            // 从 target 获取 symbol 以获取 typeParams
            SymbolInfo* sym = nullptr;
            if (auto ident = dynamic_cast<const IdentExpr*>(methodCall->target.get())) {
                if (ident->scope) sym = ident->scope->lookup(ident->ident);
            }
            if (mname == "get" || mname == "pop") {
                return sym ? getContainerElemType(sym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            }
            if (mname == "len") return TypeInfo{ SymbolKind::Int, {}, 0 };
            return TypeInfo{ SymbolKind::Int, {}, 0 }; // push/set return void, but we report Int
        }
        if (targetType.kind == SymbolKind::Map) {
            const std::string& mname = methodCall->method->ident;
            SymbolInfo* sym = nullptr;
            if (auto ident = dynamic_cast<const IdentExpr*>(methodCall->target.get())) {
                if (ident->scope) sym = ident->scope->lookup(ident->ident);
            }
            if (mname == "get") {
                return sym ? getContainerValueType(sym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            }
            if (mname == "contains") return TypeInfo{ SymbolKind::Int, {}, 0 };
            if (mname == "len") return TypeInfo{ SymbolKind::Int, {}, 0 };
            return TypeInfo{ SymbolKind::Int, {}, 0 };
        }
        if (targetType.kind == SymbolKind::Deque) {
            const std::string& mname = methodCall->method->ident;
            SymbolInfo* sym = nullptr;
            if (auto ident = dynamic_cast<const IdentExpr*>(methodCall->target.get())) {
                if (ident->scope) sym = ident->scope->lookup(ident->ident);
            }
            if (mname == "get" || mname == "front" || mname == "back" || mname == "pop_front" || mname == "pop_back") {
                return sym ? getContainerElemType(sym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            }
            if (mname == "len") return TypeInfo{ SymbolKind::Int, {}, 0 };
            return TypeInfo{ SymbolKind::Int, {}, 0 };
        }
        if (targetType.kind == SymbolKind::Queue) {
            const std::string& mname = methodCall->method->ident;
            SymbolInfo* sym = nullptr;
            if (auto ident = dynamic_cast<const IdentExpr*>(methodCall->target.get())) {
                if (ident->scope) sym = ident->scope->lookup(ident->ident);
            }
            if (mname == "front" || mname == "back" || mname == "pop") {
                return sym ? getContainerElemType(sym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            }
            if (mname == "len") return TypeInfo{ SymbolKind::Int, {}, 0 };
            return TypeInfo{ SymbolKind::Int, {}, 0 };
        }
        if (targetType.kind == SymbolKind::Channel) {
            const std::string& mname = methodCall->method->ident;
            SymbolInfo* sym = nullptr;
            if (auto ident = dynamic_cast<const IdentExpr*>(methodCall->target.get())) {
                if (ident->scope) sym = ident->scope->lookup(ident->ident);
            }
            if (mname == "recv") {
                return sym ? getContainerElemType(sym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            }
            if (mname == "len") return TypeInfo{ SymbolKind::Int, {}, 0 };
            return TypeInfo{ SymbolKind::Int, {}, 0 };
        }
        std::string className = targetType.className;
        if (targetType.pointerLevel > 0 && targetType.kind == SymbolKind::Class) {
            className = targetType.className;
        }
        auto retIt = classMethodReturnTypes.find(className);
        if (retIt != classMethodReturnTypes.end()) {
            auto mit = retIt->second.find(methodCall->method->ident);
            if (mit != retIt->second.end()) return mit->second;
        }
        return TypeInfo{ SymbolKind::Int, {}, 0 };
    }
    if (auto funcCall = dynamic_cast<const FuncCallExpr*>(expr)) {
        if (funcCall->name && funcCall->name->scope) {
            SymbolInfo* sym = funcCall->name->scope->lookup(funcCall->name->ident);
            if (sym && sym->kind == SymbolKind::Function) {
                return sym->returnType.kind == SymbolKind::Invalid ? TypeInfo{ SymbolKind::Int, {}, 0 } : sym->returnType;
            }
        }
    }
    return TypeInfo{ SymbolKind::Int, {}, 0 };
}

// ===== RAII 清理实现 =====

void emitCleanupForEntry(CodeGenContext& ctx, const CleanupEntry& entry)
{
    llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);

    switch (entry.kind) {
        case CleanupKind::String: {
            // 加载 string struct，提取 data 指针，非空则 free
            llvm::StructType* strTy = getL25StringType(ctx.context);
            llvm::Value* strVal = ctx.builder.CreateLoad(strTy, entry.addr, "cleanup.str");
            llvm::Value* dataPtr = ctx.builder.CreateExtractValue(strVal, 1, "cleanup.str.data");

            llvm::BasicBlock* freeBB = llvm::BasicBlock::Create(ctx.context, "cleanup.str.free", func);
            llvm::BasicBlock* contBB = llvm::BasicBlock::Create(ctx.context, "cleanup.str.cont", func);

            llvm::Value* isNull = ctx.builder.CreateICmpEQ(
                dataPtr, llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)));
            ctx.builder.CreateCondBr(isNull, contBB, freeBB);

            ctx.builder.SetInsertPoint(freeBB);
            llvm::FunctionCallee freeFn = ctx.module.getOrInsertFunction("free",
                llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), {i8PtrTy}, false));
            ctx.builder.CreateCall(freeFn, {dataPtr});
            ctx.builder.CreateBr(contBB);

            ctx.builder.SetInsertPoint(contBB);
            break;
        }
        case CleanupKind::ClassPtr: {
            // GC 模式：内联弹出根栈（替代 l25_gc_remove_root 函数调用）
            emitInlineRootPop(ctx);
            break;
        }
        case CleanupKind::Vector:
        case CleanupKind::Map:
        case CleanupKind::Deque:
        case CleanupKind::Queue:
        case CleanupKind::Channel: {
            // 加载容器指针，非空则调用对应的 destroy
            llvm::Value* ptr = ctx.builder.CreateLoad(i8PtrTy, entry.addr, "cleanup.container");

            llvm::BasicBlock* destroyBB = llvm::BasicBlock::Create(ctx.context, "cleanup.container.del", func);
            llvm::BasicBlock* contBB = llvm::BasicBlock::Create(ctx.context, "cleanup.container.cont", func);

            llvm::Value* isNull = ctx.builder.CreateICmpEQ(
                ptr, llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)));
            ctx.builder.CreateCondBr(isNull, contBB, destroyBB);

            ctx.builder.SetInsertPoint(destroyBB);
            std::string destroyFnName;
            switch (entry.kind) {
                case CleanupKind::Vector: destroyFnName = "l25_vector_destroy"; break;
                case CleanupKind::Map:    destroyFnName = "l25_map_destroy"; break;
                case CleanupKind::Deque:  destroyFnName = "l25_deque_destroy"; break;
                case CleanupKind::Queue:   destroyFnName = "l25_queue_destroy"; break;
                case CleanupKind::Channel: destroyFnName = "l25_channel_destroy"; break;
                default: break;
            }
            llvm::FunctionCallee destroyFn = ctx.module.getOrInsertFunction(destroyFnName,
                llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), {i8PtrTy}, false));
            ctx.builder.CreateCall(destroyFn, {ptr});
            ctx.builder.CreateBr(contBB);

            ctx.builder.SetInsertPoint(contBB);
            break;
        }
        case CleanupKind::GCRoot: {
            // GC 根注销（用于 this 指针 / 类指针参数）：内联弹出根栈
            emitInlineRootPop(ctx);
            break;
        }
    }
}

void emitScopeCleanup(CodeGenContext& ctx)
{
    if (ctx.cleanupStack.empty()) return;
    auto& entries = ctx.cleanupStack.back();
    // 逆序清理
    for (auto it = entries.rbegin(); it != entries.rend(); ++it) {
        emitCleanupForEntry(ctx, *it);
    }
    ctx.popCleanupScope();
}

void emitReturnCleanup(CodeGenContext& ctx)
{
    // 为所有活跃作用域生成清理代码（内层到外层），但不弹出
    for (int i = static_cast<int>(ctx.cleanupStack.size()) - 1; i >= 0; --i) {
        auto& entries = ctx.cleanupStack[i];
        for (auto it = entries.rbegin(); it != entries.rend(); ++it) {
            emitCleanupForEntry(ctx, *it);
        }
    }
}

llvm::Value* emitStringDeepCopy(llvm::Value* strVal, CodeGenContext& ctx)
{
    llvm::StructType* strTy = getL25StringType(ctx.context);
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::Type* i64Ty = llvm::Type::getInt64Ty(ctx.context);

    llvm::Value* len = ctx.builder.CreateExtractValue(strVal, 0, "dcopy.len");
    llvm::Value* data = ctx.builder.CreateExtractValue(strVal, 1, "dcopy.data");

    llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* copyBB = llvm::BasicBlock::Create(ctx.context, "dcopy.copy", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx.context, "dcopy.merge", func);

    llvm::Value* isNull = ctx.builder.CreateICmpEQ(data,
        llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)));
    llvm::BasicBlock* entryBB = ctx.builder.GetInsertBlock();
    ctx.builder.CreateCondBr(isNull, mergeBB, copyBB);

    // 拷贝分支：malloc(len+1) + memcpy
    ctx.builder.SetInsertPoint(copyBB);
    llvm::Value* lenI64 = ctx.builder.CreateSExt(len, i64Ty, "dcopy.len64");
    llvm::Value* allocSize = ctx.builder.CreateAdd(lenI64,
        llvm::ConstantInt::get(i64Ty, 1), "dcopy.size");
    llvm::FunctionCallee mallocFn = ctx.module.getOrInsertFunction("malloc",
        llvm::FunctionType::get(i8PtrTy, {i64Ty}, false));
    llvm::Value* newData = ctx.builder.CreateCall(mallocFn, {allocSize}, "dcopy.buf");
    llvm::FunctionCallee memcpyFn = ctx.module.getOrInsertFunction("memcpy",
        llvm::FunctionType::get(i8PtrTy, {i8PtrTy, i8PtrTy, i64Ty}, false));
    ctx.builder.CreateCall(memcpyFn, {newData, data, allocSize});
    llvm::BasicBlock* copyDoneBB = ctx.builder.GetInsertBlock();
    ctx.builder.CreateBr(mergeBB);

    // 合并：PHI 选择数据指针
    ctx.builder.SetInsertPoint(mergeBB);
    llvm::PHINode* phiData = ctx.builder.CreatePHI(i8PtrTy, 2, "dcopy.phi");
    phiData->addIncoming(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)), entryBB);
    phiData->addIncoming(newData, copyDoneBB);

    llvm::Value* result = llvm::UndefValue::get(strTy);
    result = ctx.builder.CreateInsertValue(result, len, 0, "dcopy.set_len");
    result = ctx.builder.CreateInsertValue(result, phiData, 1, "dcopy.set_data");
    return result;
}

void emitStringFree(llvm::Value* strAddr, CodeGenContext& ctx)
{
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::StructType* strTy = getL25StringType(ctx.context);
    llvm::Value* oldStr = ctx.builder.CreateLoad(strTy, strAddr, "sfree.old");
    llvm::Value* oldData = ctx.builder.CreateExtractValue(oldStr, 1, "sfree.data");

    llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* freeBB = llvm::BasicBlock::Create(ctx.context, "sfree.do", func);
    llvm::BasicBlock* contBB = llvm::BasicBlock::Create(ctx.context, "sfree.done", func);

    llvm::Value* isNull = ctx.builder.CreateICmpEQ(oldData,
        llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)));
    ctx.builder.CreateCondBr(isNull, contBB, freeBB);

    ctx.builder.SetInsertPoint(freeBB);
    llvm::FunctionCallee freeFn = ctx.module.getOrInsertFunction("free",
        llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), {i8PtrTy}, false));
    ctx.builder.CreateCall(freeFn, {oldData});
    ctx.builder.CreateBr(contBB);

    ctx.builder.SetInsertPoint(contBB);
}

bool isOwnedStringExpr(const Expr* expr)
{
    // 字符串拼接产生独立 malloc 缓冲区
    if (auto* bin = dynamic_cast<const BinaryExpr*>(expr)) {
        TypeInfo lt = evaluateExprType(bin->lhs.get());
        TypeInfo rt = evaluateExprType(bin->rhs.get());
        if (lt.kind == SymbolKind::String || rt.kind == SymbolKind::String) {
            return true;
        }
    }
    // 函数/方法调用返回的字符串保证拥有所有权的缓冲区
    if (dynamic_cast<const FuncCallExpr*>(expr)) return true;
    if (dynamic_cast<const MethodCallExpr*>(expr)) return true;
    return false;
}

void emitClassPtrFree(llvm::Value* ptrAddr, const std::string& className, CodeGenContext& ctx)
{
    auto it = classStructTypes.find(className);
    if (it == classStructTypes.end()) return;
    llvm::StructType* classTy = it->second;
    llvm::PointerType* classPtrTy = llvm::PointerType::get(classTy, 0);
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);

    llvm::Value* ptr = ctx.builder.CreateLoad(classPtrTy, ptrAddr, "cpfree.old");

    llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* delBB = llvm::BasicBlock::Create(ctx.context, "cpfree.do", func);
    llvm::BasicBlock* contBB = llvm::BasicBlock::Create(ctx.context, "cpfree.done", func);

    llvm::Value* isNull = ctx.builder.CreateICmpEQ(ptr, llvm::ConstantPointerNull::get(classPtrTy));
    ctx.builder.CreateCondBr(isNull, contBB, delBB);

    ctx.builder.SetInsertPoint(delBB);
    std::string dtorName = buildDtorName(className);
    if (llvm::Function* dtorFunc = ctx.module.getFunction(dtorName)) {
        ctx.builder.CreateCall(dtorFunc, {ptr});
    }
    llvm::FunctionCallee freeFn = ctx.module.getOrInsertFunction("free",
        llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), {i8PtrTy}, false));
    ctx.builder.CreateCall(freeFn, {ctx.builder.CreateBitCast(ptr, i8PtrTy)});
    ctx.builder.CreateBr(contBB);

    ctx.builder.SetInsertPoint(contBB);
}

// ===== 容器运行时支持 =====

void ensureContainerRuntimeDeclared(CodeGenContext& ctx)
{
    auto* voidTy = llvm::Type::getVoidTy(ctx.context);
    auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    auto* i32Ty = llvm::Type::getInt32Ty(ctx.context);
    auto* i64Ty = llvm::Type::getInt64Ty(ctx.context);

    // Vector API
    if (!ctx.module.getFunction("l25_vector_create")) {
        ctx.module.getOrInsertFunction("l25_vector_create",
            llvm::FunctionType::get(i8PtrTy, {i64Ty}, false));
    }
    if (!ctx.module.getFunction("l25_vector_destroy")) {
        ctx.module.getOrInsertFunction("l25_vector_destroy",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_vector_push")) {
        ctx.module.getOrInsertFunction("l25_vector_push",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_vector_pop")) {
        ctx.module.getOrInsertFunction("l25_vector_pop",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_vector_get")) {
        ctx.module.getOrInsertFunction("l25_vector_get",
            llvm::FunctionType::get(i8PtrTy, {i8PtrTy, i64Ty}, false));
    }
    if (!ctx.module.getFunction("l25_vector_set")) {
        ctx.module.getOrInsertFunction("l25_vector_set",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i64Ty, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_vector_len")) {
        ctx.module.getOrInsertFunction("l25_vector_len",
            llvm::FunctionType::get(i64Ty, {i8PtrTy}, false));
    }

    // Map API
    if (!ctx.module.getFunction("l25_map_create")) {
        ctx.module.getOrInsertFunction("l25_map_create",
            llvm::FunctionType::get(i8PtrTy, {i64Ty, i64Ty, i32Ty}, false));
    }
    if (!ctx.module.getFunction("l25_map_destroy")) {
        ctx.module.getOrInsertFunction("l25_map_destroy",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_map_set")) {
        ctx.module.getOrInsertFunction("l25_map_set",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_map_get")) {
        ctx.module.getOrInsertFunction("l25_map_get",
            llvm::FunctionType::get(i8PtrTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_map_contains")) {
        ctx.module.getOrInsertFunction("l25_map_contains",
            llvm::FunctionType::get(i32Ty, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_map_erase")) {
        ctx.module.getOrInsertFunction("l25_map_erase",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_map_len")) {
        ctx.module.getOrInsertFunction("l25_map_len",
            llvm::FunctionType::get(i64Ty, {i8PtrTy}, false));
    }

    // Deque API
    if (!ctx.module.getFunction("l25_deque_create")) {
        ctx.module.getOrInsertFunction("l25_deque_create",
            llvm::FunctionType::get(i8PtrTy, {i64Ty}, false));
    }
    if (!ctx.module.getFunction("l25_deque_destroy")) {
        ctx.module.getOrInsertFunction("l25_deque_destroy",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_push_front")) {
        ctx.module.getOrInsertFunction("l25_deque_push_front",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_push_back")) {
        ctx.module.getOrInsertFunction("l25_deque_push_back",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_pop_front")) {
        ctx.module.getOrInsertFunction("l25_deque_pop_front",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_pop_back")) {
        ctx.module.getOrInsertFunction("l25_deque_pop_back",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_get")) {
        ctx.module.getOrInsertFunction("l25_deque_get",
            llvm::FunctionType::get(i8PtrTy, {i8PtrTy, i64Ty}, false));
    }
    if (!ctx.module.getFunction("l25_deque_set")) {
        ctx.module.getOrInsertFunction("l25_deque_set",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i64Ty, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_front")) {
        ctx.module.getOrInsertFunction("l25_deque_front",
            llvm::FunctionType::get(i8PtrTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_back")) {
        ctx.module.getOrInsertFunction("l25_deque_back",
            llvm::FunctionType::get(i8PtrTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_deque_len")) {
        ctx.module.getOrInsertFunction("l25_deque_len",
            llvm::FunctionType::get(i64Ty, {i8PtrTy}, false));
    }

    // Queue API
    if (!ctx.module.getFunction("l25_queue_create")) {
        ctx.module.getOrInsertFunction("l25_queue_create",
            llvm::FunctionType::get(i8PtrTy, {i64Ty}, false));
    }
    if (!ctx.module.getFunction("l25_queue_destroy")) {
        ctx.module.getOrInsertFunction("l25_queue_destroy",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_queue_push")) {
        ctx.module.getOrInsertFunction("l25_queue_push",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_queue_pop")) {
        ctx.module.getOrInsertFunction("l25_queue_pop",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_queue_front")) {
        ctx.module.getOrInsertFunction("l25_queue_front",
            llvm::FunctionType::get(i8PtrTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_queue_back")) {
        ctx.module.getOrInsertFunction("l25_queue_back",
            llvm::FunctionType::get(i8PtrTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_queue_len")) {
        ctx.module.getOrInsertFunction("l25_queue_len",
            llvm::FunctionType::get(i64Ty, {i8PtrTy}, false));
    }

    // Channel API
    if (!ctx.module.getFunction("l25_channel_create")) {
        ctx.module.getOrInsertFunction("l25_channel_create",
            llvm::FunctionType::get(i8PtrTy, {i64Ty, i64Ty}, false));
    }
    if (!ctx.module.getFunction("l25_channel_destroy")) {
        ctx.module.getOrInsertFunction("l25_channel_destroy",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_channel_send")) {
        ctx.module.getOrInsertFunction("l25_channel_send",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_channel_recv")) {
        ctx.module.getOrInsertFunction("l25_channel_recv",
            llvm::FunctionType::get(voidTy, {i8PtrTy, i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_channel_len")) {
        ctx.module.getOrInsertFunction("l25_channel_len",
            llvm::FunctionType::get(i64Ty, {i8PtrTy}, false));
    }
    if (!ctx.module.getFunction("l25_channel_close")) {
        ctx.module.getOrInsertFunction("l25_channel_close",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }
}

uint64_t getTypeAllocSize(const TypeInfo& typeInfo, CodeGenContext& ctx)
{
    llvm::Type* ty = typeInfoToLLVMValueType(typeInfo, ctx.context);
    if (!ty) return 4; // fallback to i32
    return ctx.module.getDataLayout().getTypeAllocSize(ty);
}

int32_t getMapKeyTypeTag(const TypeInfo& keyType)
{
    if (keyType.kind == SymbolKind::Int) return 0;   // L25_KEY_INT
    if (keyType.kind == SymbolKind::Float) return 1;  // L25_KEY_FLOAT
    if (keyType.kind == SymbolKind::String) return 2;  // L25_KEY_STRING
    if (keyType.pointerLevel > 0) return 3;            // L25_KEY_PTR
    return 4;                                          // L25_KEY_OTHER
}

TypeInfo getContainerElemType(const SymbolInfo* symbol)
{
    if (!symbol) return TypeInfo{ SymbolKind::Int, {}, 0, false };
    if ((symbol->kind == SymbolKind::Vector || symbol->kind == SymbolKind::Deque
         || symbol->kind == SymbolKind::Queue || symbol->kind == SymbolKind::Channel)
        && !symbol->typeParams.empty()) {
        return symbol->typeParams[0];
    }
    return TypeInfo{ SymbolKind::Int, {}, 0, false };
}

TypeInfo getContainerKeyType(const SymbolInfo* symbol)
{
    if (!symbol) return TypeInfo{ SymbolKind::Int, {}, 0, false };
    if (symbol->kind == SymbolKind::Map && !symbol->typeParams.empty()) {
        return symbol->typeParams[0];
    }
    return TypeInfo{ SymbolKind::Int, {}, 0, false };
}

TypeInfo getContainerValueType(const SymbolInfo* symbol)
{
    if (!symbol) return TypeInfo{ SymbolKind::Int, {}, 0, false };
    if (symbol->kind == SymbolKind::Map && symbol->typeParams.size() >= 2) {
        return symbol->typeParams[1];
    }
    return TypeInfo{ SymbolKind::Int, {}, 0, false };
}

// ===== GC 运行时支持 =====

void ensureGCRuntimeDeclared(CodeGenContext& ctx)
{
    auto* voidTy  = llvm::Type::getVoidTy(ctx.context);
    auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    auto* i64Ty   = llvm::Type::getInt64Ty(ctx.context);
    // void** 即 i8** → 用 i8PtrTy 的指针
    auto* i8PtrPtrTy = llvm::PointerType::get(i8PtrTy, 0);

    // l25_gc_init(void)
    if (!ctx.module.getFunction("l25_gc_init")) {
        ctx.module.getOrInsertFunction("l25_gc_init",
            llvm::FunctionType::get(voidTy, {}, false));
    }
    // l25_gc_shutdown(void)
    if (!ctx.module.getFunction("l25_gc_shutdown")) {
        ctx.module.getOrInsertFunction("l25_gc_shutdown",
            llvm::FunctionType::get(voidTy, {}, false));
    }
    // l25_gc_alloc(size, scan_fn, dtor_fn) → i8*
    if (!ctx.module.getFunction("l25_gc_alloc")) {
        ctx.module.getOrInsertFunction("l25_gc_alloc",
            llvm::FunctionType::get(i8PtrTy, {i64Ty, i8PtrTy, i8PtrTy}, false));
    }
    // l25_gc_add_root(void** root)
    if (!ctx.module.getFunction("l25_gc_add_root")) {
        ctx.module.getOrInsertFunction("l25_gc_add_root",
            llvm::FunctionType::get(voidTy, {i8PtrPtrTy}, false));
    }
    // l25_gc_remove_root(void** root)
    if (!ctx.module.getFunction("l25_gc_remove_root")) {
        ctx.module.getOrInsertFunction("l25_gc_remove_root",
            llvm::FunctionType::get(voidTy, {i8PtrPtrTy}, false));
    }
    // l25_gc_collect(void)
    if (!ctx.module.getFunction("l25_gc_collect")) {
        ctx.module.getOrInsertFunction("l25_gc_collect",
            llvm::FunctionType::get(voidTy, {}, false));
    }
    // l25_gc_free(i8*)
    if (!ctx.module.getFunction("l25_gc_free")) {
        ctx.module.getOrInsertFunction("l25_gc_free",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }
    // l25_gc_write_barrier(i8*) — 写屏障
    if (!ctx.module.getFunction("l25_gc_write_barrier")) {
        ctx.module.getOrInsertFunction("l25_gc_write_barrier",
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false));
    }

    // ===== 线程安全根栈函数 =====
    // l25_gc_root_push(i8**)
    if (!ctx.module.getFunction("l25_gc_root_push")) {
        ctx.module.getOrInsertFunction("l25_gc_root_push",
            llvm::FunctionType::get(voidTy, {i8PtrPtrTy}, false));
    }
    // l25_gc_root_pop(void)
    if (!ctx.module.getFunction("l25_gc_root_pop")) {
        ctx.module.getOrInsertFunction("l25_gc_root_pop",
            llvm::FunctionType::get(voidTy, {}, false));
    }
    // l25_gc_thread_init(void)
    if (!ctx.module.getFunction("l25_gc_thread_init")) {
        ctx.module.getOrInsertFunction("l25_gc_thread_init",
            llvm::FunctionType::get(voidTy, {}, false));
    }
    // l25_gc_thread_fini(void)
    if (!ctx.module.getFunction("l25_gc_thread_fini")) {
        ctx.module.getOrInsertFunction("l25_gc_thread_fini",
            llvm::FunctionType::get(voidTy, {}, false));
    }
}

// ===== 根栈 push（通过函数调用，线程安全） =====
void emitInlineRootPush(llvm::Value* allocaAddr, CodeGenContext& ctx)
{
    ensureGCRuntimeDeclared(ctx);
    auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);

    // cast alloca 地址为 i8**
    auto* i8PtrPtrTy = llvm::PointerType::get(i8PtrTy, 0);
    llvm::Value* castedAddr = ctx.builder.CreateBitCast(allocaAddr, i8PtrPtrTy, "gc.root.cast");

    // call l25_gc_root_push(castedAddr)
    auto* pushFn = ctx.module.getFunction("l25_gc_root_push");
    ctx.builder.CreateCall(pushFn, {castedAddr});
}

// ===== 根栈 pop（通过函数调用，线程安全） =====
void emitInlineRootPop(CodeGenContext& ctx)
{
    ensureGCRuntimeDeclared(ctx);

    // call l25_gc_root_pop()
    auto* popFn = ctx.module.getFunction("l25_gc_root_pop");
    ctx.builder.CreateCall(popFn, {});
}

void emitGCScanFunction(CodeGenContext& ctx, const std::string& className)
{
    auto layoutIt = classFieldLayouts.find(className);
    if (layoutIt == classFieldLayouts.end()) return;

    // 检查此类是否有需要扫描的指针字段（类指针或基本类型指针）
    bool hasPointerFields = false;
    for (const auto& [fname, ftype] : layoutIt->second) {
        if (ftype.pointerLevel > 0) {
            hasPointerFields = true;
            break;
        }
    }

    auto* i8PtrTy   = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    auto* voidTy    = llvm::Type::getVoidTy(ctx.context);
    // mark_fn 类型：void (i8*)
    auto* markFnTy  = llvm::FunctionType::get(voidTy, {i8PtrTy}, false);
    auto* markFnPtrTy = llvm::PointerType::get(markFnTy, 0);
    // scan 函数类型：void (i8*, void(*)(i8*))
    auto* scanFnTy  = llvm::FunctionType::get(voidTy, {i8PtrTy, markFnPtrTy}, false);

    std::string fnName = "__gc_scan_" + className;

    // 如果没有需要扫描的指针字段，不生成 scan 函数（分配时传 null）
    if (!hasPointerFields) return;

    // 避免重复生成
    if (ctx.module.getFunction(fnName)) return;

    llvm::Function* scanFn = llvm::Function::Create(
        scanFnTy, llvm::Function::InternalLinkage, fnName, ctx.module);

    // 保存当前 builder 状态
    llvm::BasicBlock* savedBB = ctx.builder.GetInsertBlock();
    llvm::BasicBlock::iterator savedPt = ctx.builder.GetInsertPoint();

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx.context, "entry", scanFn);
    ctx.builder.SetInsertPoint(entry);

    auto argIt = scanFn->arg_begin();
    llvm::Value* objPtr  = &*argIt; objPtr->setName("obj");
    llvm::Value* markFn  = &*(argIt + 1); markFn->setName("mark_fn");

    llvm::StructType* structTy = classStructTypes[className];
    llvm::Value* typedPtr = ctx.builder.CreateBitCast(
        objPtr, llvm::PointerType::get(structTy, 0), "typed");

    for (size_t i = 0; i < layoutIt->second.size(); i++) {
        const auto& [fname, ftype] = layoutIt->second[i];
        if (ftype.pointerLevel > 0) {
            // 获取字段指针
            llvm::Value* fieldPtr = ctx.builder.CreateStructGEP(
                structTy, typedPtr, static_cast<unsigned>(i), "field." + fname);
            // 加载字段值（指针类型）
            llvm::Type* fieldValTy = llvm::PointerType::get(
                llvm::Type::getInt8Ty(ctx.context), 0);
            llvm::Value* fieldVal = ctx.builder.CreateLoad(fieldValTy, fieldPtr, "load." + fname);
            // 检查非空
            llvm::BasicBlock* markBB = llvm::BasicBlock::Create(
                ctx.context, "mark." + fname, scanFn);
            llvm::BasicBlock* skipBB = llvm::BasicBlock::Create(
                ctx.context, "skip." + fname, scanFn);
            llvm::Value* isNull = ctx.builder.CreateICmpEQ(
                fieldVal,
                llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(fieldValTy)));
            ctx.builder.CreateCondBr(isNull, skipBB, markBB);

            ctx.builder.SetInsertPoint(markBB);
            llvm::Value* castVal = ctx.builder.CreateBitCast(fieldVal, i8PtrTy, "cast." + fname);
            ctx.builder.CreateCall(markFnTy, markFn, {castVal});
            ctx.builder.CreateBr(skipBB);

            ctx.builder.SetInsertPoint(skipBB);
        }
    }

    ctx.builder.CreateRetVoid();

    // 恢复之前的 builder 状态
    if (savedBB) {
        ctx.builder.SetInsertPoint(savedBB, savedPt);
    }
}