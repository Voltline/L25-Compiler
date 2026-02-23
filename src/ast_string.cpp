#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

// ===== 字符串字面量节点 =====
StringLiteralExpr::StringLiteralExpr(const std::string& val) : value(val) {}

void StringLiteralExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "String(\"" << value << "\")" << std::endl;
}

llvm::Value* StringLiteralExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::StructType* strTy = getL25StringType(ctx.context);
    // 创建全局常量字符串（含 null 终止符）
    llvm::Constant* strConst = ctx.builder.CreateGlobalString(value, ".str");
    // 构建 __l25_string { len, data }
    llvm::Value* result = llvm::UndefValue::get(strTy);
    llvm::Value* lenVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context),
                                                  static_cast<int>(value.size()));
    result = ctx.builder.CreateInsertValue(result, lenVal, 0, "str_set_len");
    result = ctx.builder.CreateInsertValue(result, strConst, 1, "str_set_data");
    return result;
}

// ===== strlen 内建函数节点 =====
StrlenExpr::StrlenExpr(std::unique_ptr<Expr> target) : target(std::move(target)) {}

void StrlenExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Strlen" << std::endl;
    if (target) target->print(indent + 2);
}

llvm::Value* StrlenExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* val = target->codeGen(ctx);
    if (!val) {
        reportError("strlen 参数表达式生成失败");
        return nullptr;
    }
    llvm::StructType* strTy = getL25StringType(ctx.context);
    if (val->getType() != strTy) {
        reportError("strlen 仅支持字符串类型参数");
        return nullptr;
    }
    // 直接提取 len 字段（index 0）
    return ctx.builder.CreateExtractValue(val, 0, "str_len");
}

// ===== readln 内建函数节点 =====

void ReadlnExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Readln" << std::endl;
}

llvm::Value* ReadlnExpr::codeGen(CodeGenContext& ctx) const
{
    ensureStringRuntimeDeclared(ctx);

    auto* i32Ty    = llvm::Type::getInt32Ty(ctx.context);

    llvm::AllocaInst* outLen = ctx.builder.CreateAlloca(i32Ty, nullptr, "readln.outlen");
    llvm::Function* fn = ctx.module.getFunction("l25_readln");
    llvm::Value* data = ctx.builder.CreateCall(fn, { outLen }, "readln.data");
    llvm::Value* len  = ctx.builder.CreateLoad(i32Ty, outLen, "readln.len");

    llvm::StructType* strTy = getL25StringType(ctx.context);
    llvm::Value* result = llvm::UndefValue::get(strTy);
    result = ctx.builder.CreateInsertValue(result, len, 0, "str.set.len");
    result = ctx.builder.CreateInsertValue(result, data, 1, "str.set.data");
    return result;
}

// ===== itos 内建函数节点 =====

ItosExpr::ItosExpr(std::unique_ptr<Expr> value) : value(std::move(value)) {}

void ItosExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Itos" << std::endl;
    if (value) value->print(indent + 2);
}

llvm::Value* ItosExpr::codeGen(CodeGenContext& ctx) const
{
    ensureStringRuntimeDeclared(ctx);

    llvm::Value* val = value->codeGen(ctx);
    if (!val) { reportError("itos 参数表达式生成失败"); return nullptr; }

    auto* i32Ty = llvm::Type::getInt32Ty(ctx.context);
    llvm::Value* intVal = castValueToType(val, i32Ty, ctx);

    llvm::AllocaInst* outLen = ctx.builder.CreateAlloca(i32Ty, nullptr, "itos.outlen");
    llvm::Function* fn = ctx.module.getFunction("l25_itos");
    llvm::Value* data = ctx.builder.CreateCall(fn, { intVal, outLen }, "itos.data");
    llvm::Value* len  = ctx.builder.CreateLoad(i32Ty, outLen, "itos.len");

    llvm::StructType* strTy = getL25StringType(ctx.context);
    llvm::Value* result = llvm::UndefValue::get(strTy);
    result = ctx.builder.CreateInsertValue(result, len, 0, "str.set.len");
    result = ctx.builder.CreateInsertValue(result, data, 1, "str.set.data");
    return result;
}
