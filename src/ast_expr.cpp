#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/DataLayout.h>
#include <algorithm>

// ===== Bool表达式节点 =====
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

    llvm::Type* lhsTy = lhsVal->getType();
    llvm::Type* rhsTy = rhsVal->getType();

    // 字符串比较
    llvm::StructType* strTy = getL25StringType(ctx.context);
    bool lhsIsStr = (lhsTy == strTy);
    bool rhsIsStr = (rhsTy == strTy);
    if (lhsIsStr || rhsIsStr) {
        if (!lhsIsStr || !rhsIsStr) {
            reportError("字符串只能与字符串比较");
            return nullptr;
        }
        if (symbol != "==" && symbol != "!=") {
            reportError("字符串仅支持 == 和 != 比较");
            return nullptr;
        }
        ensureStringRuntimeDeclared(ctx);
        llvm::Value* lhsData = ctx.builder.CreateExtractValue(lhsVal, 1, "lhs_str_data");
        llvm::Value* rhsData = ctx.builder.CreateExtractValue(rhsVal, 1, "rhs_str_data");
        llvm::Value* cmpResult = ctx.builder.CreateCall(
            ctx.module.getFunction("strcmp"), { lhsData, rhsData }, "strcmp_result");
        if (symbol == "==") {
            return ctx.builder.CreateICmpEQ(cmpResult,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0), "str_eq");
        } else {
            return ctx.builder.CreateICmpNE(cmpResult,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0), "str_ne");
        }
    }

    bool useFloat = lhsTy->isFloatingPointTy() || rhsTy->isFloatingPointTy();
    bool usePointer = lhsTy->isPointerTy() || rhsTy->isPointerTy();

    if (usePointer && !useFloat) {
        llvm::Type* targetPtrTy = lhsTy->isPointerTy() ? lhsTy : rhsTy;
        lhsVal = castValueToType(lhsVal, targetPtrTy, ctx);
        rhsVal = castValueToType(rhsVal, targetPtrTy, ctx);
        if (!lhsVal || !rhsVal) {
            reportError("布尔表达式左右子表达式生成失败");
            return nullptr;
        }
        if (symbol == "==") {
            return ctx.builder.CreateICmpEQ(lhsVal, rhsVal, "ptr_cmpeq");
        } else if (symbol == "!=") {
            return ctx.builder.CreateICmpNE(lhsVal, rhsVal, "ptr_cmpne");
        }
        reportError("指针仅支持==和!=比较");
        return nullptr;
    }

    if (useFloat) {
        lhsVal = castValueToType(lhsVal, llvm::Type::getFloatTy(ctx.context), ctx);
        rhsVal = castValueToType(rhsVal, llvm::Type::getFloatTy(ctx.context), ctx);
        if (symbol == "==") {
            return ctx.builder.CreateFCmpOEQ(lhsVal, rhsVal, "fcmp_eq");
        } else if (symbol == "!=") {
            return ctx.builder.CreateFCmpONE(lhsVal, rhsVal, "fcmp_ne");
        } else if (symbol == "<") {
            return ctx.builder.CreateFCmpOLT(lhsVal, rhsVal, "fcmp_lt");
        } else if (symbol == "<=") {
            return ctx.builder.CreateFCmpOLE(lhsVal, rhsVal, "fcmp_le");
        } else if (symbol == ">") {
            return ctx.builder.CreateFCmpOGT(lhsVal, rhsVal, "fcmp_gt");
        } else if (symbol == ">=") {
            return ctx.builder.CreateFCmpOGE(lhsVal, rhsVal, "fcmp_ge");
        }
        reportError("不支持的布尔操作符");
        return nullptr;
    }

    lhsVal = castValueToType(lhsVal, llvm::Type::getInt32Ty(ctx.context), ctx);
    rhsVal = castValueToType(rhsVal, llvm::Type::getInt32Ty(ctx.context), ctx);
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
    }

    reportError("不支持的布尔操作符");
    return nullptr;
}

// ===== 整数常量节点 =====
NumberExpr::NumberExpr(int val) : value(val) {}

void NumberExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Number(" << value << ")" << std::endl;
}

llvm::Value* NumberExpr::codeGen(CodeGenContext& ctx) const
{
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), value);
}

// ===== 浮点常量节点 =====
FloatNumberExpr::FloatNumberExpr(double val) : value(val) {}

void FloatNumberExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Float(" << value << ")" << std::endl;
}

llvm::Value* FloatNumberExpr::codeGen(CodeGenContext& ctx) const
{
    return llvm::ConstantFP::get(llvm::Type::getFloatTy(ctx.context), value);
}

// ===== Nil =====
void NilExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Nil" << std::endl;
}

llvm::Value* NilExpr::codeGen(CodeGenContext& ctx) const
{
    auto* ptrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    return llvm::ConstantPointerNull::get(ptrTy);
}

// ===== 一元运算符节点 =====
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
        if (RHS->getType()->isFloatingPointTy()) {
            return ctx.builder.CreateFNeg(RHS);
        }
        return ctx.builder.CreateNeg(RHS);
    default:
        return nullptr;
    }
}

// ===== 取地址表达式节点 =====
AddressOfExpr::AddressOfExpr(std::unique_ptr<Expr> target)
    : target(std::move(target)) {}

void AddressOfExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "AddressOf" << std::endl;
    target->print(indent + 2);
}

llvm::Value* AddressOfExpr::codeGen(CodeGenContext& ctx) const
{
    if (auto ident = dynamic_cast<IdentExpr*>(target.get())) {
        SymbolInfo* symbol = scope->lookup(ident->ident);
        if (!symbol || !symbol->addr) {
            reportError("变量未声明或未分配空间: " + ident->ident);
            return nullptr;
        }
        if (symbol->kind == SymbolKind::Array) {
            // 取数组首元素地址
            std::vector<llvm::Value*> indices;
            indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
            for (size_t i = 0; i < symbol->dimensions.size(); ++i) {
                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
            }
            llvm::Type* arrayType = buildArrayType(llvm::Type::getInt32Ty(ctx.context), symbol->dimensions);
            return ctx.builder.CreateGEP(arrayType, symbol->addr, indices, ident->ident + "_addr");
        }
        return symbol->addr;
    }
    if (auto subscript = dynamic_cast<ArraySubscriptExpr*>(target.get())) {
        return subscript->getAddress(ctx);
    }
    reportError("无法对该表达式取地址");
    return nullptr;
}

// ===== 解引用表达式节点 =====
DereferenceExpr::DereferenceExpr(std::unique_ptr<Expr> pointerExpr)
    : pointerExpr(std::move(pointerExpr)) {}

void DereferenceExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Deref" << std::endl;
    pointerExpr->print(indent + 2);
}

llvm::Value* DereferenceExpr::getPointerValue(CodeGenContext& ctx) const
{
    llvm::Value* ptrVal = pointerExpr->codeGen(ctx);
    if (!ptrVal || !ptrVal->getType()->isPointerTy()) {
        reportError("尝试解引用非指针类型");
        return nullptr;
    }
    return ptrVal;
}

llvm::Value* DereferenceExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* ptrVal = getPointerValue(ctx);
    if (!ptrVal) return nullptr;
    TypeInfo pointeeInfo = evaluateExprType(this);
    llvm::Type* loadType = typeInfoToLLVMValueType(pointeeInfo, ctx.context);
    return ctx.builder.CreateLoad(loadType, ptrVal, "deref");
}

// ===== 二元运算符节点 =====
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

    // 字符串拼接
    llvm::StructType* strTy = getL25StringType(ctx.context);
    bool lhsIsStr = (LHS->getType() == strTy);
    bool rhsIsStr = (RHS->getType() == strTy);
    if (lhsIsStr || rhsIsStr) {
        if (op != '+') {
            reportError("字符串仅支持 + 运算（拼接）");
            return nullptr;
        }
        if (!lhsIsStr || !rhsIsStr) {
            reportError("字符串拼接要求两侧均为字符串类型");
            return nullptr;
        }
        ensureStringRuntimeDeclared(ctx);
        auto* i64Ty = llvm::Type::getInt64Ty(ctx.context);
        auto* i32Ty = llvm::Type::getInt32Ty(ctx.context);

        // 提取各自的 len 和 data
        llvm::Value* lhsLen = ctx.builder.CreateExtractValue(LHS, 0, "lhs_len");
        llvm::Value* lhsData = ctx.builder.CreateExtractValue(LHS, 1, "lhs_data");
        llvm::Value* rhsLen = ctx.builder.CreateExtractValue(RHS, 0, "rhs_len");
        llvm::Value* rhsData = ctx.builder.CreateExtractValue(RHS, 1, "rhs_data");

        // newLen = lhsLen + rhsLen
        llvm::Value* newLen = ctx.builder.CreateAdd(lhsLen, rhsLen, "new_len");
        // allocSize = newLen + 1 (for null terminator)
        llvm::Value* allocSize = ctx.builder.CreateAdd(newLen,
            llvm::ConstantInt::get(i32Ty, 1), "alloc_size");
        llvm::Value* allocSize64 = ctx.builder.CreateZExt(allocSize, i64Ty, "alloc_size64");

        // buf = malloc(allocSize)
        llvm::Value* buf = ctx.builder.CreateCall(
            ctx.module.getFunction("malloc"), { allocSize64 }, "concat_buf");

        // memcpy(buf, lhsData, lhsLen)
        llvm::Value* lhsLen64 = ctx.builder.CreateZExt(lhsLen, i64Ty, "lhs_len64");
        ctx.builder.CreateCall(ctx.module.getFunction("memcpy"),
            { buf, lhsData, lhsLen64 });

        // memcpy(buf + lhsLen, rhsData, rhsLen)
        llvm::Value* rhsLen64 = ctx.builder.CreateZExt(rhsLen, i64Ty, "rhs_len64");
        llvm::Value* bufOffset = ctx.builder.CreateGEP(
            llvm::Type::getInt8Ty(ctx.context), buf, { lhsLen64 }, "buf_offset");
        // copy rhsLen + 1 to include null terminator
        llvm::Value* rhsCopyLen = ctx.builder.CreateAdd(rhsLen64,
            llvm::ConstantInt::get(i64Ty, 1), "rhs_copy_len");
        ctx.builder.CreateCall(ctx.module.getFunction("memcpy"),
            { bufOffset, rhsData, rhsCopyLen });

        // 构建结果 __l25_string
        llvm::Value* result = llvm::UndefValue::get(strTy);
        result = ctx.builder.CreateInsertValue(result, newLen, 0, "concat_set_len");
        result = ctx.builder.CreateInsertValue(result, buf, 1, "concat_set_data");

        // 释放中间拼接产生的临时缓冲区（链式 a + b + c 中的中间结果）
        auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::FunctionCallee freeFn = ctx.module.getOrInsertFunction("free",
            llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), {i8PtrTy}, false));
        if (isOwnedStringExpr(lhs.get())) {
            llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
            llvm::BasicBlock* freeLhsBB = llvm::BasicBlock::Create(ctx.context, "concat.free.lhs", func);
            llvm::BasicBlock* contLhsBB = llvm::BasicBlock::Create(ctx.context, "concat.cont.lhs", func);
            llvm::Value* lhsNull = ctx.builder.CreateICmpEQ(lhsData, llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)));
            ctx.builder.CreateCondBr(lhsNull, contLhsBB, freeLhsBB);
            ctx.builder.SetInsertPoint(freeLhsBB);
            ctx.builder.CreateCall(freeFn, {lhsData});
            ctx.builder.CreateBr(contLhsBB);
            ctx.builder.SetInsertPoint(contLhsBB);
        }
        if (isOwnedStringExpr(rhs.get())) {
            llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
            llvm::BasicBlock* freeRhsBB = llvm::BasicBlock::Create(ctx.context, "concat.free.rhs", func);
            llvm::BasicBlock* contRhsBB = llvm::BasicBlock::Create(ctx.context, "concat.cont.rhs", func);
            llvm::Value* rhsNull = ctx.builder.CreateICmpEQ(rhsData, llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy)));
            ctx.builder.CreateCondBr(rhsNull, contRhsBB, freeRhsBB);
            ctx.builder.SetInsertPoint(freeRhsBB);
            ctx.builder.CreateCall(freeFn, {rhsData});
            ctx.builder.CreateBr(contRhsBB);
            ctx.builder.SetInsertPoint(contRhsBB);
        }

        return result;
    }

    bool useFloat = LHS->getType()->isFloatingPointTy() || RHS->getType()->isFloatingPointTy();
    if (useFloat) {
        LHS = castValueToType(LHS, llvm::Type::getFloatTy(ctx.context), ctx);
        RHS = castValueToType(RHS, llvm::Type::getFloatTy(ctx.context), ctx);
        switch (op) {
        case '+':
            return ctx.builder.CreateFAdd(LHS, RHS, "faddtmp");
        case '-':
            return ctx.builder.CreateFSub(LHS, RHS, "fsubtmp");
        case '*':
            return ctx.builder.CreateFMul(LHS, RHS, "fmultmp");
        case '/':
            return ctx.builder.CreateFDiv(LHS, RHS, "fdivtmp");
        case '%':
            reportError("浮点数不支持取模运算");
            return nullptr;
        default:
            reportError("不支持的二元运算符: " + std::string(1, op));
            return nullptr;
        }
    }

    LHS = castValueToType(LHS, llvm::Type::getInt32Ty(ctx.context), ctx);
    RHS = castValueToType(RHS, llvm::Type::getInt32Ty(ctx.context), ctx);
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

// ===== 数组下标访问运算节点 =====
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
    llvm::Type* elementType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
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

    llvm::Type* valueType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
    return ctx.builder.CreateLoad(valueType, gep, "load_elem");
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
    llvm::Type* elementType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
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

// ===== 成员访问表达式 =====
MemberAccessExpr::MemberAccessExpr(std::unique_ptr<Expr> target, std::unique_ptr<IdentExpr> member)
    : target(std::move(target))
    , member(std::move(member)) {}

void MemberAccessExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "MemberAccess" << std::endl;
    target->print(indent + 2);
    member->print(indent + 2);
}

llvm::Value* MemberAccessExpr::getPointer(CodeGenContext& ctx) const
{
    TypeInfo baseType = evaluateExprType(target.get());
        llvm::Value* baseValue = nullptr;
    if (auto ident = dynamic_cast<IdentExpr*>(target.get())) {
        if (SymbolInfo* symbol = ident->scope->lookup(ident->ident)) {
            if (symbol->kind == SymbolKind::Class && symbol->pointerLevel == 0) {
                baseValue = symbol->addr;
            }
        }
    } else if (auto memberAccess = dynamic_cast<MemberAccessExpr*>(target.get())) {
        baseValue = memberAccess->getPointer(ctx);
    } else if (auto arrayAccess = dynamic_cast<ArraySubscriptExpr*>(target.get())) {
        baseValue = arrayAccess->getAddress(ctx);
    }

    if (!baseValue) {
        baseValue = target->codeGen(ctx);
    }

    if (baseType.pointerLevel > 0) {
        baseType.pointerLevel -= 1;
    }
    auto layoutIt = classFieldLayouts.find(baseType.className);
    if (layoutIt == classFieldLayouts.end()) {
        reportError("无法找到类布局：" + baseType.className);
        return nullptr;
    }
    int index = -1;
    for (size_t i = 0; i < layoutIt->second.size(); ++i) {
        if (layoutIt->second[i].first == member->ident) {
            index = static_cast<int>(i);
            break;
        }
    }
    if (index < 0) {
        reportError("成员不存在：" + member->ident);
        return nullptr;
    }

    llvm::Value* ptr = baseValue;
    if (!ptr->getType()->isPointerTy()) {
        auto* tmp = ctx.builder.CreateAlloca(ptr->getType());
        ctx.builder.CreateStore(ptr, tmp);
        ptr = tmp;
    }

    llvm::StructType* structTy = classStructTypes[baseType.className];
    if (!structTy) {
        reportError("无法找到类类型：" + baseType.className);
        return nullptr;
    }

    llvm::PointerType* targetPtrTy = llvm::PointerType::get(structTy, 0);
    if (ptr->getType() != targetPtrTy) {
        ptr = ctx.builder.CreateBitCast(ptr, targetPtrTy);
    }
    return ctx.builder.CreateStructGEP(structTy, ptr, index, "fieldptr");
}

llvm::Value* MemberAccessExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* ptr = getPointer(ctx);
    if (!ptr) return nullptr;
    TypeInfo baseType = evaluateExprType(target.get());
    if (baseType.pointerLevel > 0) {
        baseType.pointerLevel -= 1;
    }
    auto layoutIt = classFieldLayouts.find(baseType.className);
    if (layoutIt == classFieldLayouts.end()) {
        reportError("无法找到类布局：" + baseType.className);
        return nullptr;
    }
    int index = -1;
    for (size_t i = 0; i < layoutIt->second.size(); ++i) {
        if (layoutIt->second[i].first == member->ident) {
            index = static_cast<int>(i);
            break;
        }
    }
    if (index < 0) {
        reportError("成员不存在：" + member->ident);
        return nullptr;
    }

    llvm::Type* fieldTy = typeInfoToLLVMType(layoutIt->second[index].second, ctx.context, true);
    if (!fieldTy) {
        reportError("无法得到字段类型：" + member->ident);
        return nullptr;
    }

    return ctx.builder.CreateLoad(fieldTy, ptr, "fieldload");
}

// ===== 方法调用 =====
MethodCallExpr::MethodCallExpr(std::unique_ptr<Expr> target, std::unique_ptr<IdentExpr> method, std::unique_ptr<ArgList> args)
    : target(std::move(target))
    , method(std::move(method))
    , args(std::move(args)) {}

void MethodCallExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "MethodCall" << std::endl;
    target->print(indent + 2);
    method->print(indent + 2);
    if (args) args->print(indent + 2);
}

llvm::Value* MethodCallExpr::codeGen(CodeGenContext& ctx) const
{
    TypeInfo baseType = evaluateExprType(target.get());
        llvm::Value* baseValue = nullptr;
    if (auto ident = dynamic_cast<IdentExpr*>(target.get())) {
        if (SymbolInfo* symbol = ident->scope->lookup(ident->ident)) {
            if (symbol->kind == SymbolKind::Class && symbol->pointerLevel == 0) {
                baseValue = symbol->addr;
            }
        }
    } else if (auto memberAccess = dynamic_cast<MemberAccessExpr*>(target.get())) {
        baseValue = memberAccess->getPointer(ctx);
    } else if (auto arrayAccess = dynamic_cast<ArraySubscriptExpr*>(target.get())) {
        baseValue = arrayAccess->getAddress(ctx);
    }

    if (!baseValue) {
        baseValue = target->codeGen(ctx);
    }
    llvm::StructType* structTy = classStructTypes[baseType.className];
    if (!structTy) {
        reportError("无法找到方法所属的类类型");
        return nullptr;
    }
    llvm::Value* thisPtr = baseValue;
    if (!thisPtr->getType()->isPointerTy()) {
        auto* tmp = ctx.builder.CreateAlloca(thisPtr->getType());
        ctx.builder.CreateStore(thisPtr, tmp);
        thisPtr = tmp;
    }
    llvm::PointerType* targetPtrTy = llvm::PointerType::get(structTy, 0);
    if (thisPtr->getType() != targetPtrTy) {
        thisPtr = ctx.builder.CreateBitCast(thisPtr, targetPtrTy);
    }
    std::vector<llvm::Value*> callArgs;
    callArgs.push_back(thisPtr);
    if (args) {
        for (const auto& arg : args->args) {
            callArgs.push_back(arg->codeGen(ctx));
        }
    }
    std::string funcName = baseType.className + "." + method->ident;
    llvm::Function* callee = ctx.module.getFunction(funcName);
    if (!callee) {
        reportError("未找到方法定义：" + funcName);
        return nullptr;
    }
    return ctx.builder.CreateCall(callee, callArgs, "methodcall");
}

// ===== new 表达式 =====
NewExpr::NewExpr(std::unique_ptr<IdentExpr> className, std::unique_ptr<ArgList> args)
    : className(std::move(className))
    , args(std::move(args)) {}

void NewExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "NewExpr(" << className->ident << ")" << std::endl;
    if (args) args->print(indent + 2);
}

llvm::Value* NewExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::StructType* classTy = classStructTypes[className->ident];
    if (!classTy) {
        reportError("无法找到类类型：" + className->ident);
        return nullptr;
    }

    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::Type* sizeTy = llvm::Type::getInt64Ty(ctx.context);
    llvm::FunctionCallee mallocFn = ctx.module.getOrInsertFunction(
        "malloc",
        llvm::FunctionType::get(i8PtrTy, { sizeTy }, false)
    );

    uint64_t allocSize = ctx.module.getDataLayout().getTypeAllocSize(classTy);
    llvm::Value* sizeVal = llvm::ConstantInt::get(sizeTy, allocSize);
    llvm::Value* rawPtr = ctx.builder.CreateCall(mallocFn, { sizeVal }, "rawobj");
    llvm::Value* typedPtr = ctx.builder.CreateBitCast(rawPtr, llvm::PointerType::get(classTy, 0), "obj");

    size_t argCount = args ? args->args.size() : 0;
    std::string ctorName = buildCtorName(className->ident, argCount);
    if (llvm::Function* ctorFunc = ctx.module.getFunction(ctorName)) {
        std::vector<llvm::Value*> callArgs;
        callArgs.push_back(typedPtr);
        if (args) {
            for (size_t i = 0; i < args->args.size(); ++i) {
                llvm::Value* argVal = args->args[i]->codeGen(ctx);
                if (!argVal) return nullptr;
                llvm::Type* expectedType = ctorFunc->getFunctionType()->getParamType(static_cast<unsigned>(i + 1));
                argVal = castValueToType(argVal, expectedType, ctx);
                callArgs.push_back(argVal);
            }
        }
        ctx.builder.CreateCall(ctorFunc, callArgs);
    } else {
        if (argCount > 0) {
            reportError("未找到匹配的构造函数：" + ctorName);
            return nullptr;
        }
        auto memsetFn = llvm::Intrinsic::getDeclaration(
            &ctx.module,
            llvm::Intrinsic::memset,
            { llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0), llvm::Type::getInt64Ty(ctx.context) }
        );
        llvm::Value* zeroVal = llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::Value* isVolatile = llvm::ConstantInt::getFalse(ctx.context);
        ctx.builder.CreateCall(memsetFn, {
            ctx.builder.CreateBitCast(typedPtr, llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0)),
            zeroVal,
            sizeVal,
            isVolatile
        });
    }

    return typedPtr;
}

// ===== 标识符节点 =====
IdentExpr::IdentExpr(const std::string& ident, TypeInfo type) : ident(ident), type(type) {}

void IdentExpr::print(int indent) const 
{
    switch (type.kind) {
    case SymbolKind::Int:
    case SymbolKind::Float:
    case SymbolKind::Function:
    case SymbolKind::Program:
        std::cout << std::string(indent, ' ')
            << "Ident(" << ident << ": "
            << SymbolName[static_cast<int>(type.kind)] << ")" << std::endl;
        break;
    case SymbolKind::Pointer:
        std::cout << std::string(indent, ' ')
            << "Ident(" << ident << ": Pointer^" << std::max(1, type.pointerLevel) << ")" << std::endl;
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
    if (symbol->kind == SymbolKind::Int || symbol->kind == SymbolKind::Float) {
        llvm::Type* valueType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
        return ctx.builder.CreateLoad(valueType, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::Pointer) {
        TypeInfo symbolType{ SymbolKind::Pointer, {}, symbol->pointerLevel, symbol->isFloat };
        llvm::Type* valueType = typeInfoToLLVMValueType(symbolType, ctx.context);
        return ctx.builder.CreateLoad(valueType, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::Class) {
        TypeInfo symbolType{ SymbolKind::Class, symbol->dimensions, symbol->pointerLevel, symbol->isFloat, symbol->className };
        llvm::Type* valueType = typeInfoToLLVMValueType(symbolType, ctx.context);
        return ctx.builder.CreateLoad(valueType, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::String) {
        llvm::Type* strTy = getL25StringType(ctx.context);
        return ctx.builder.CreateLoad(strTy, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::Array) {
        return symbol->addr;
    }

    reportError("不支持返回的标识符: " + ident);
    return nullptr;
}
