#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include "ast.h"
#include "parser.tab.hpp"

extern int yyparse();
Expr* rootExpr = nullptr;

int main() 
{
    // 解析输入的表达式
    yyparse();

    if (!rootExpr) {
        std::cerr << "No expression parsed.\n";
        return 1;
    }

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder(context);
    llvm::Module module("toy_module", context);

    // 声明 printf 函数
    llvm::FunctionType *printfType = llvm::FunctionType::get(
        builder.getInt32Ty(),  // 返回值类型是 int
        {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)},  // 参数类型是 const char*
        true  // 外部链接
    );
    llvm::FunctionCallee printfFunc = module.getOrInsertFunction("printf", printfType);

    // 创建 int main() 函数
    llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", module);

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);

    // 生成表达式并将其值打印出来
    llvm::Value* val = rootExpr->codeGen(builder, context, module);
    if (!val) {
        std::cerr << "Code generation failed.\n";
        return 1;
    }

    // 将结果转换为字符串并调用 printf
    llvm::Value* formatStr = builder.CreateGlobalStringPtr("Result: %d\n");

    std::vector<llvm::Value*> args;
    args.push_back(formatStr);
    args.push_back(val);  // 将表达式的值作为 printf 的参数

    builder.CreateCall(printfFunc, args);  // 调用 printf

    llvm::Value* retVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);

    builder.CreateRet(retVal);  // return 表达式的值

    // 输出完整 IR 到标准输出
    module.print(llvm::outs(), nullptr);  // 输出完整 IR

    // 清理资源
    delete rootExpr;

    return 0;
}