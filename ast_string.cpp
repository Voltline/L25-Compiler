#include "include/ast.h"
#include "include/codegen_utils.h"
#include "include/errorReporter.h"
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
