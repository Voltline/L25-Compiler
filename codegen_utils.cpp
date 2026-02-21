#include "include/ast.h"
#include "include/codegen_utils.h"
#include "include/errorReporter.h"
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
    return TypeInfo{ symbol->kind, symbol->dimensions, symbol->pointerLevel, symbol->isFloat, symbol->className };
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
    if (auto methodCall = dynamic_cast<const MethodCallExpr*>(expr)) {
        TypeInfo targetType = evaluateExprType(methodCall->target.get());
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
