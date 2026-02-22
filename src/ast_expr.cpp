#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/DataLayout.h>
#include <algorithm>

// ===== Bool表达式节点 =====
// 比较运算构造函数
BoolExpr::BoolExpr(std::string symbol, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
    : symbol(symbol)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs)) {}

// 逻辑二元运算构造函数 (&&, ||)
BoolExpr::BoolExpr(std::string symbol, std::unique_ptr<BoolExpr> bool_lhs, std::unique_ptr<BoolExpr> bool_rhs)
    : symbol(symbol)
    , bool_lhs(std::move(bool_lhs))
    , bool_rhs(std::move(bool_rhs)) {}

// 逻辑一元运算构造函数 (!)
BoolExpr::BoolExpr(std::string symbol, std::unique_ptr<BoolExpr> operand)
    : symbol(symbol)
    , bool_lhs(std::move(operand)) {}

void BoolExpr::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Bool(" << symbol << ")" << std::endl;
    if (lhs) lhs->print(indent + 2);
    if (rhs) rhs->print(indent + 2);
    if (bool_lhs) bool_lhs->print(indent + 2);
    if (bool_rhs) bool_rhs->print(indent + 2);
}

llvm::Value* BoolExpr::codeGen(CodeGenContext& ctx) const
{
    // ===== 逻辑非 (!) =====
    if (symbol == "!") {
        llvm::Value* operandVal = bool_lhs->codeGen(ctx);
        if (!operandVal) { reportError("! 操作数生成失败"); return nullptr; }
        return ctx.builder.CreateNot(operandVal, "lnot");
    }

    // ===== 逻辑与 (&&) — 短路求值 =====
    if (symbol == "&&") {
        llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
        llvm::BasicBlock* rhsBB = llvm::BasicBlock::Create(ctx.context, "and.rhs", func);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx.context, "and.merge", func);

        llvm::Value* lhsVal = bool_lhs->codeGen(ctx);
        if (!lhsVal) { reportError("&& 左操作数生成失败"); return nullptr; }
        llvm::BasicBlock* lhsBB = ctx.builder.GetInsertBlock();
        ctx.builder.CreateCondBr(lhsVal, rhsBB, mergeBB);

        ctx.builder.SetInsertPoint(rhsBB);
        llvm::Value* rhsVal = bool_rhs->codeGen(ctx);
        if (!rhsVal) { reportError("&& 右操作数生成失败"); return nullptr; }
        llvm::BasicBlock* rhsDoneBB = ctx.builder.GetInsertBlock();
        ctx.builder.CreateBr(mergeBB);

        ctx.builder.SetInsertPoint(mergeBB);
        llvm::PHINode* phi = ctx.builder.CreatePHI(llvm::Type::getInt1Ty(ctx.context), 2, "and.result");
        phi->addIncoming(llvm::ConstantInt::getFalse(ctx.context), lhsBB);
        phi->addIncoming(rhsVal, rhsDoneBB);
        return phi;
    }

    // ===== 逻辑或 (||) — 短路求值 =====
    if (symbol == "||") {
        llvm::Function* func = ctx.builder.GetInsertBlock()->getParent();
        llvm::BasicBlock* rhsBB = llvm::BasicBlock::Create(ctx.context, "or.rhs", func);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx.context, "or.merge", func);

        llvm::Value* lhsVal = bool_lhs->codeGen(ctx);
        if (!lhsVal) { reportError("|| 左操作数生成失败"); return nullptr; }
        llvm::BasicBlock* lhsBB = ctx.builder.GetInsertBlock();
        ctx.builder.CreateCondBr(lhsVal, mergeBB, rhsBB);

        ctx.builder.SetInsertPoint(rhsBB);
        llvm::Value* rhsVal = bool_rhs->codeGen(ctx);
        if (!rhsVal) { reportError("|| 右操作数生成失败"); return nullptr; }
        llvm::BasicBlock* rhsDoneBB = ctx.builder.GetInsertBlock();
        ctx.builder.CreateBr(mergeBB);

        ctx.builder.SetInsertPoint(mergeBB);
        llvm::PHINode* phi = ctx.builder.CreatePHI(llvm::Type::getInt1Ty(ctx.context), 2, "or.result");
        phi->addIncoming(llvm::ConstantInt::getTrue(ctx.context), lhsBB);
        phi->addIncoming(rhsVal, rhsDoneBB);
        return phi;
    }

    // ===== 比较运算（原有逻辑）=====
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

    // ===== Vector 下标读取 =====
    if (symbol->kind == SymbolKind::Vector) {
        ensureContainerRuntimeDeclared(ctx);
        llvm::Value* containerPtr = ctx.builder.CreateLoad(
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            symbol->addr, "vec.load");
        llvm::Value* idx = subscript[0]->codeGen(ctx);
        idx = castValueToType(idx, llvm::Type::getInt64Ty(ctx.context), ctx);
        llvm::FunctionCallee fn = ctx.module.getFunction("l25_vector_get");
        llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr, idx}, "vec.sub.ptr");
        TypeInfo elemType = getContainerElemType(symbol);
        llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);
        llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
        return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "vec.sub.val");
    }

    // ===== Map 下标读取 =====
    if (symbol->kind == SymbolKind::Map) {
        ensureContainerRuntimeDeclared(ctx);
        llvm::Value* containerPtr = ctx.builder.CreateLoad(
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            symbol->addr, "map.load");
        TypeInfo keyType = getContainerKeyType(symbol);
        TypeInfo valType = getContainerValueType(symbol);
        llvm::Type* keyLLVMTy = typeInfoToLLVMValueType(keyType, ctx.context);
        llvm::Type* valLLVMTy = typeInfoToLLVMValueType(valType, ctx.context);
        auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::Value* keyVal = subscript[0]->codeGen(ctx);
        keyVal = castValueToType(keyVal, keyLLVMTy, ctx);
        llvm::AllocaInst* keyTmp = ctx.builder.CreateAlloca(keyLLVMTy, nullptr, "map.sub.key");
        ctx.builder.CreateStore(keyVal, keyTmp);
        llvm::Value* keyPtr = ctx.builder.CreateBitCast(keyTmp, i8PtrTy);
        llvm::FunctionCallee fn = ctx.module.getFunction("l25_map_get");
        llvm::Value* valPtr = ctx.builder.CreateCall(fn, {containerPtr, keyPtr}, "map.sub.ptr");
        llvm::Value* typedPtr = ctx.builder.CreateBitCast(valPtr, llvm::PointerType::get(valLLVMTy, 0));
        return ctx.builder.CreateLoad(valLLVMTy, typedPtr, "map.sub.val");
    }

    // ===== Deque 下标读取 =====
    if (symbol->kind == SymbolKind::Deque) {
        ensureContainerRuntimeDeclared(ctx);
        llvm::Value* containerPtr = ctx.builder.CreateLoad(
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            symbol->addr, "deq.load");
        llvm::Value* idx = subscript[0]->codeGen(ctx);
        idx = castValueToType(idx, llvm::Type::getInt64Ty(ctx.context), ctx);
        llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_get");
        llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr, idx}, "deq.sub.ptr");
        TypeInfo elemType = getContainerElemType(symbol);
        llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);
        llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
        return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "deq.sub.val");
    }

    // ===== 指针下标访问（new T[n] 返回的堆指针）=====
    if (symbol->pointerLevel > 0 && symbol->kind == SymbolKind::Pointer) {
        llvm::Type* elemTy = symbol->isFloat
            ? llvm::Type::getFloatTy(ctx.context)
            : llvm::Type::getInt32Ty(ctx.context);
        llvm::Type* ptrTy = llvm::PointerType::get(elemTy, 0);
        llvm::Value* basePtr = ctx.builder.CreateLoad(ptrTy, symbol->addr, "ptr.load");
        llvm::Value* idx = subscript[0]->codeGen(ctx);
        if (!idx) return nullptr;
        llvm::Value* gep = ctx.builder.CreateGEP(elemTy, basePtr, {idx}, "ptr.elem");
        return ctx.builder.CreateLoad(elemTy, gep, "ptr.elem.val");
    }

    // ===== 原始数组下标 =====

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

    // ===== Vector 下标地址（用于 v[i] = x）=====
    if (symbol->kind == SymbolKind::Vector) {
        ensureContainerRuntimeDeclared(ctx);
        llvm::Value* containerPtr = ctx.builder.CreateLoad(
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            symbol->addr, "vec.addr.load");
        llvm::Value* idx = subscript[0]->codeGen(ctx);
        idx = castValueToType(idx, llvm::Type::getInt64Ty(ctx.context), ctx);
        llvm::FunctionCallee fn = ctx.module.getFunction("l25_vector_get");
        llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr, idx}, "vec.addr.ptr");
        TypeInfo elemType = getContainerElemType(symbol);
        llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);
        return ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
    }

    // ===== Map 下标地址（用于 m[k] = v）=====
    if (symbol->kind == SymbolKind::Map) {
        ensureContainerRuntimeDeclared(ctx);
        llvm::Value* containerPtr = ctx.builder.CreateLoad(
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            symbol->addr, "map.addr.load");
        TypeInfo keyType = getContainerKeyType(symbol);
        TypeInfo valType = getContainerValueType(symbol);
        llvm::Type* keyLLVMTy = typeInfoToLLVMValueType(keyType, ctx.context);
        llvm::Type* valLLVMTy = typeInfoToLLVMValueType(valType, ctx.context);
        auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::Value* keyVal = subscript[0]->codeGen(ctx);
        keyVal = castValueToType(keyVal, keyLLVMTy, ctx);
        llvm::AllocaInst* keyTmp = ctx.builder.CreateAlloca(keyLLVMTy, nullptr, "map.addr.key");
        ctx.builder.CreateStore(keyVal, keyTmp);
        llvm::Value* keyPtr = ctx.builder.CreateBitCast(keyTmp, i8PtrTy);
        // l25_map_get auto-inserts if key doesn't exist
        llvm::FunctionCallee fn = ctx.module.getFunction("l25_map_get");
        llvm::Value* valPtr = ctx.builder.CreateCall(fn, {containerPtr, keyPtr}, "map.addr.ptr");
        return ctx.builder.CreateBitCast(valPtr, llvm::PointerType::get(valLLVMTy, 0));
    }

    // ===== Deque 下标地址（用于 d[i] = x）=====
    if (symbol->kind == SymbolKind::Deque) {
        ensureContainerRuntimeDeclared(ctx);
        llvm::Value* containerPtr = ctx.builder.CreateLoad(
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            symbol->addr, "deq.addr.load");
        llvm::Value* idx = subscript[0]->codeGen(ctx);
        idx = castValueToType(idx, llvm::Type::getInt64Ty(ctx.context), ctx);
        llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_get");
        llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr, idx}, "deq.addr.ptr");
        TypeInfo elemType = getContainerElemType(symbol);
        llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);
        return ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
    }

    // ===== 指针下标地址（new T[n] 返回的堆指针）=====
    if (symbol->pointerLevel > 0 && symbol->kind == SymbolKind::Pointer) {
        llvm::Type* elemTy = symbol->isFloat
            ? llvm::Type::getFloatTy(ctx.context)
            : llvm::Type::getInt32Ty(ctx.context);
        llvm::Type* ptrTy = llvm::PointerType::get(elemTy, 0);
        llvm::Value* basePtr = ctx.builder.CreateLoad(ptrTy, symbol->addr, "ptr.addr.load");
        llvm::Value* idx = subscript[0]->codeGen(ctx);
        if (!idx) return nullptr;
        return ctx.builder.CreateGEP(elemTy, basePtr, {idx}, "ptr.addr.elem");
    }

    // ===== 原始数组下标地址 =====

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
        // 如果中间成员是指针类型（如 n1.next 的 next: *Node），
        // 需要 load 得到指针值，而非字段地址
        TypeInfo memberType = evaluateExprType(target.get());
        if (memberType.kind == SymbolKind::Class && memberType.pointerLevel > 0) {
            baseValue = memberAccess->codeGen(ctx);
        } else {
            baseValue = memberAccess->getPointer(ctx);
        }
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

    // ===== 容器方法调用分发 =====
    if (baseType.kind == SymbolKind::Vector || baseType.kind == SymbolKind::Map
        || baseType.kind == SymbolKind::Deque || baseType.kind == SymbolKind::Queue) {
        ensureContainerRuntimeDeclared(ctx);
        // 获取容器指针 (i8*)
        llvm::Value* containerPtr = target->codeGen(ctx);
        if (!containerPtr) { reportError("容器变量无效"); return nullptr; }

        // 获取 symbol 以获取 typeParams
        SymbolInfo* containerSym = nullptr;
        if (auto ident = dynamic_cast<IdentExpr*>(target.get())) {
            if (ident->scope) containerSym = ident->scope->lookup(ident->ident);
        }

        auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
        auto* i32Ty = llvm::Type::getInt32Ty(ctx.context);
        auto* i64Ty = llvm::Type::getInt64Ty(ctx.context);

        const std::string& mname = method->ident;

        if (baseType.kind == SymbolKind::Vector) {
            TypeInfo elemType = containerSym ? getContainerElemType(containerSym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);

            if (mname == "push") {
                // push(elem): alloca elem, store, pass ptr
                llvm::Value* elemVal = args->args[0]->codeGen(ctx);
                elemVal = castValueToType(elemVal, elemLLVMTy, ctx);
                llvm::AllocaInst* tmp = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "vec.push.tmp");
                ctx.builder.CreateStore(elemVal, tmp);
                llvm::Value* tmpCast = ctx.builder.CreateBitCast(tmp, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_vector_push");
                ctx.builder.CreateCall(fn, {containerPtr, tmpCast});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "pop") {
                // pop(): alloca out, call, load and return
                llvm::AllocaInst* out = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "vec.pop.out");
                llvm::Value* outCast = ctx.builder.CreateBitCast(out, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_vector_pop");
                ctx.builder.CreateCall(fn, {containerPtr, outCast});
                return ctx.builder.CreateLoad(elemLLVMTy, out, "vec.pop.val");
            } else if (mname == "get") {
                // get(index): returns ptr to elem, load it
                llvm::Value* idx = args->args[0]->codeGen(ctx);
                idx = castValueToType(idx, i64Ty, ctx);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_vector_get");
                llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr, idx}, "vec.get.ptr");
                llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
                return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "vec.get.val");
            } else if (mname == "set") {
                // set(index, value)
                llvm::Value* idx = args->args[0]->codeGen(ctx);
                idx = castValueToType(idx, i64Ty, ctx);
                llvm::Value* elemVal = args->args[1]->codeGen(ctx);
                elemVal = castValueToType(elemVal, elemLLVMTy, ctx);
                llvm::AllocaInst* tmp = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "vec.set.tmp");
                ctx.builder.CreateStore(elemVal, tmp);
                llvm::Value* tmpCast = ctx.builder.CreateBitCast(tmp, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_vector_set");
                ctx.builder.CreateCall(fn, {containerPtr, idx, tmpCast});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "len") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_vector_len");
                llvm::Value* len64 = ctx.builder.CreateCall(fn, {containerPtr}, "vec.len");
                return ctx.builder.CreateTrunc(len64, i32Ty, "vec.len.i32");
            }
        } else if (baseType.kind == SymbolKind::Map) {
            TypeInfo keyType = containerSym ? getContainerKeyType(containerSym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            TypeInfo valType = containerSym ? getContainerValueType(containerSym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            llvm::Type* keyLLVMTy = typeInfoToLLVMValueType(keyType, ctx.context);
            llvm::Type* valLLVMTy = typeInfoToLLVMValueType(valType, ctx.context);

            // 辅助 lambda: 将 key 值存入 alloca 并返回 i8* 指针
            auto emitKeyPtr = [&](llvm::Value* keyVal) -> llvm::Value* {
                keyVal = castValueToType(keyVal, keyLLVMTy, ctx);
                llvm::AllocaInst* tmp = ctx.builder.CreateAlloca(keyLLVMTy, nullptr, "map.key.tmp");
                ctx.builder.CreateStore(keyVal, tmp);
                return ctx.builder.CreateBitCast(tmp, i8PtrTy);
            };

            if (mname == "set") {
                // set(key, value)
                llvm::Value* keyVal = args->args[0]->codeGen(ctx);
                llvm::Value* keyPtr = emitKeyPtr(keyVal);
                llvm::Value* valVal = args->args[1]->codeGen(ctx);
                valVal = castValueToType(valVal, valLLVMTy, ctx);
                llvm::AllocaInst* valTmp = ctx.builder.CreateAlloca(valLLVMTy, nullptr, "map.val.tmp");
                ctx.builder.CreateStore(valVal, valTmp);
                llvm::Value* valPtr = ctx.builder.CreateBitCast(valTmp, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_map_set");
                ctx.builder.CreateCall(fn, {containerPtr, keyPtr, valPtr});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "get") {
                // get(key): returns ptr to value, load it
                llvm::Value* keyVal = args->args[0]->codeGen(ctx);
                llvm::Value* keyPtr = emitKeyPtr(keyVal);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_map_get");
                llvm::Value* valPtr = ctx.builder.CreateCall(fn, {containerPtr, keyPtr}, "map.get.ptr");
                llvm::Value* typedPtr = ctx.builder.CreateBitCast(valPtr, llvm::PointerType::get(valLLVMTy, 0));
                return ctx.builder.CreateLoad(valLLVMTy, typedPtr, "map.get.val");
            } else if (mname == "contains") {
                llvm::Value* keyVal = args->args[0]->codeGen(ctx);
                llvm::Value* keyPtr = emitKeyPtr(keyVal);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_map_contains");
                return ctx.builder.CreateCall(fn, {containerPtr, keyPtr}, "map.contains");
            } else if (mname == "erase") {
                llvm::Value* keyVal = args->args[0]->codeGen(ctx);
                llvm::Value* keyPtr = emitKeyPtr(keyVal);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_map_erase");
                ctx.builder.CreateCall(fn, {containerPtr, keyPtr});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "len") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_map_len");
                llvm::Value* len64 = ctx.builder.CreateCall(fn, {containerPtr}, "map.len");
                return ctx.builder.CreateTrunc(len64, i32Ty, "map.len.i32");
            }
        } else if (baseType.kind == SymbolKind::Deque) {
            TypeInfo elemType = containerSym ? getContainerElemType(containerSym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);

            if (mname == "push_front") {
                llvm::Value* elemVal = args->args[0]->codeGen(ctx);
                elemVal = castValueToType(elemVal, elemLLVMTy, ctx);
                llvm::AllocaInst* tmp = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "deq.pf.tmp");
                ctx.builder.CreateStore(elemVal, tmp);
                llvm::Value* tmpCast = ctx.builder.CreateBitCast(tmp, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_push_front");
                ctx.builder.CreateCall(fn, {containerPtr, tmpCast});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "push_back") {
                llvm::Value* elemVal = args->args[0]->codeGen(ctx);
                elemVal = castValueToType(elemVal, elemLLVMTy, ctx);
                llvm::AllocaInst* tmp = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "deq.pb.tmp");
                ctx.builder.CreateStore(elemVal, tmp);
                llvm::Value* tmpCast = ctx.builder.CreateBitCast(tmp, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_push_back");
                ctx.builder.CreateCall(fn, {containerPtr, tmpCast});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "pop_front") {
                llvm::AllocaInst* out = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "deq.pf.out");
                llvm::Value* outCast = ctx.builder.CreateBitCast(out, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_pop_front");
                ctx.builder.CreateCall(fn, {containerPtr, outCast});
                return ctx.builder.CreateLoad(elemLLVMTy, out, "deq.pf.val");
            } else if (mname == "pop_back") {
                llvm::AllocaInst* out = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "deq.pb.out");
                llvm::Value* outCast = ctx.builder.CreateBitCast(out, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_pop_back");
                ctx.builder.CreateCall(fn, {containerPtr, outCast});
                return ctx.builder.CreateLoad(elemLLVMTy, out, "deq.pb.val");
            } else if (mname == "get") {
                llvm::Value* idx = args->args[0]->codeGen(ctx);
                idx = castValueToType(idx, i64Ty, ctx);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_get");
                llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr, idx}, "deq.get.ptr");
                llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
                return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "deq.get.val");
            } else if (mname == "set") {
                llvm::Value* idx = args->args[0]->codeGen(ctx);
                idx = castValueToType(idx, i64Ty, ctx);
                llvm::Value* elemVal = args->args[1]->codeGen(ctx);
                elemVal = castValueToType(elemVal, elemLLVMTy, ctx);
                llvm::AllocaInst* tmp = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "deq.set.tmp");
                ctx.builder.CreateStore(elemVal, tmp);
                llvm::Value* tmpCast = ctx.builder.CreateBitCast(tmp, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_set");
                ctx.builder.CreateCall(fn, {containerPtr, idx, tmpCast});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "front") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_front");
                llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr}, "deq.front.ptr");
                llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
                return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "deq.front.val");
            } else if (mname == "back") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_back");
                llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr}, "deq.back.ptr");
                llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
                return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "deq.back.val");
            } else if (mname == "len") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_deque_len");
                llvm::Value* len64 = ctx.builder.CreateCall(fn, {containerPtr}, "deq.len");
                return ctx.builder.CreateTrunc(len64, i32Ty, "deq.len.i32");
            }
        } else if (baseType.kind == SymbolKind::Queue) {
            TypeInfo elemType = containerSym ? getContainerElemType(containerSym) : TypeInfo{ SymbolKind::Int, {}, 0 };
            llvm::Type* elemLLVMTy = typeInfoToLLVMValueType(elemType, ctx.context);

            if (mname == "push") {
                llvm::Value* elemVal = args->args[0]->codeGen(ctx);
                elemVal = castValueToType(elemVal, elemLLVMTy, ctx);
                llvm::AllocaInst* tmp = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "que.push.tmp");
                ctx.builder.CreateStore(elemVal, tmp);
                llvm::Value* tmpCast = ctx.builder.CreateBitCast(tmp, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_queue_push");
                ctx.builder.CreateCall(fn, {containerPtr, tmpCast});
                return llvm::ConstantInt::get(i32Ty, 0);
            } else if (mname == "pop") {
                llvm::AllocaInst* out = ctx.builder.CreateAlloca(elemLLVMTy, nullptr, "que.pop.out");
                llvm::Value* outCast = ctx.builder.CreateBitCast(out, i8PtrTy);
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_queue_pop");
                ctx.builder.CreateCall(fn, {containerPtr, outCast});
                return ctx.builder.CreateLoad(elemLLVMTy, out, "que.pop.val");
            } else if (mname == "front") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_queue_front");
                llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr}, "que.front.ptr");
                llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
                return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "que.front.val");
            } else if (mname == "back") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_queue_back");
                llvm::Value* elemPtr = ctx.builder.CreateCall(fn, {containerPtr}, "que.back.ptr");
                llvm::Value* typedPtr = ctx.builder.CreateBitCast(elemPtr, llvm::PointerType::get(elemLLVMTy, 0));
                return ctx.builder.CreateLoad(elemLLVMTy, typedPtr, "que.back.val");
            } else if (mname == "len") {
                llvm::FunctionCallee fn = ctx.module.getFunction("l25_queue_len");
                llvm::Value* len64 = ctx.builder.CreateCall(fn, {containerPtr}, "que.len");
                return ctx.builder.CreateTrunc(len64, i32Ty, "que.len.i32");
            }
        }
        reportError("未知的容器方法：" + mname);
        return nullptr;
    }

    // ===== 类方法调用（原逻辑）=====
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

    // GC 分配：l25_gc_alloc(size, scan_fn, dtor_fn)
    ensureGCRuntimeDeclared(ctx);
    uint64_t allocSize = ctx.module.getDataLayout().getTypeAllocSize(classTy);
    llvm::Value* sizeVal = llvm::ConstantInt::get(sizeTy, allocSize);

    // 获取 scan 函数（若此类有指针字段）
    llvm::Function* scanFunc = ctx.module.getFunction("__gc_scan_" + className->ident);
    llvm::Value* scanFnPtr = scanFunc
        ? ctx.builder.CreateBitCast(scanFunc, i8PtrTy)
        : llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy));

    // 获取析构函数
    std::string dtorNameStr = buildDtorName(className->ident);
    llvm::Function* dtorFunc = ctx.module.getFunction(dtorNameStr);
    llvm::Value* dtorFnPtr = dtorFunc
        ? ctx.builder.CreateBitCast(dtorFunc, i8PtrTy)
        : llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy));

    llvm::FunctionCallee gcAllocFn = ctx.module.getFunction("l25_gc_alloc");
    llvm::Value* rawPtr = ctx.builder.CreateCall(gcAllocFn, { sizeVal, scanFnPtr, dtorFnPtr }, "rawobj");
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

// ===== new T[n] 数组堆分配表达式 =====
NewArrayExpr::NewArrayExpr(const std::string& elementTypeName, bool isFloat, std::unique_ptr<Expr> sizeExpr)
    : elementTypeName(elementTypeName), isFloat(isFloat), sizeExpr(std::move(sizeExpr)) {}

void NewArrayExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "NewArray(" << elementTypeName << ")" << std::endl;
    if (sizeExpr) sizeExpr->print(indent + 2);
}

llvm::Value* NewArrayExpr::codeGen(CodeGenContext& ctx) const
{
    // 计算数组大小
    llvm::Value* sizeVal = sizeExpr->codeGen(ctx);
    if (!sizeVal) {
        reportError("new 数组大小表达式生成失败");
        return nullptr;
    }

    // 转换为 i64
    llvm::Type* i64Ty = llvm::Type::getInt64Ty(ctx.context);
    if (sizeVal->getType() != i64Ty) {
        if (sizeVal->getType()->isIntegerTy()) {
            sizeVal = ctx.builder.CreateZExt(sizeVal, i64Ty, "arr.size.ext");
        } else if (sizeVal->getType()->isFloatTy()) {
            sizeVal = ctx.builder.CreateFPToUI(sizeVal, i64Ty, "arr.size.ftoi");
        }
    }

    // 元素类型
    llvm::Type* elemTy = isFloat
        ? llvm::Type::getFloatTy(ctx.context)
        : llvm::Type::getInt32Ty(ctx.context);
    uint64_t elemSize = ctx.module.getDataLayout().getTypeAllocSize(elemTy);

    // GC 分配：l25_gc_alloc(totalSize, NULL, NULL)
    // 基本类型数组无指针字段（不需要 scan）、无析构函数
    ensureGCRuntimeDeclared(ctx);
    llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    llvm::Value* elemSizeVal = llvm::ConstantInt::get(i64Ty, elemSize);
    llvm::Value* totalSize = ctx.builder.CreateMul(sizeVal, elemSizeVal, "newarr.totalsize");
    llvm::Value* nullPtr = llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(i8PtrTy));

    llvm::FunctionCallee gcAllocFn = ctx.module.getFunction("l25_gc_alloc");
    llvm::Value* rawPtr = ctx.builder.CreateCall(gcAllocFn, {totalSize, nullPtr, nullPtr}, "newarr.raw");

    // 转换为目标指针类型
    llvm::Type* ptrTy = llvm::PointerType::get(elemTy, 0);
    llvm::Value* typedPtr = ctx.builder.CreateBitCast(rawPtr, ptrTy, "newarr.ptr");

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
    } else if (symbol->kind == SymbolKind::Vector || symbol->kind == SymbolKind::Map
            || symbol->kind == SymbolKind::Deque  || symbol->kind == SymbolKind::Queue) {
        // 容器是不透明指针 (i8*)，直接 load
        llvm::Type* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
        return ctx.builder.CreateLoad(i8PtrTy, symbol->addr, ident);
    }

    reportError("不支持返回的标识符: " + ident);
    return nullptr;
}
