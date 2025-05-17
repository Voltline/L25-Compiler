#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include "include/ast.h"
#include "include/semanticAnalysis.h"
#include "parser.tab.hpp"

extern int yyparse();
Program* rootProgram = nullptr;
bool hasError = false;

int main(int argc, const char* argv[]) 
{
    yydebug = argc != 1;
    // 解析输入的表达式
    yyparse();

    if (!rootProgram || hasError) {
        std::cerr << "语法解析失败，停止" << std::endl;
        return 1;
    }

    rootProgram->print();

    SemanticAnalyzer analyzer;
    analyzer.analyze(*rootProgram);

    if (hasError) {
        std::cerr << "语义检查出错，停止" << std::endl;
        return 1;
    } else {
        std::cout << "语义检查完成" << std::endl;
    }

    rootProgram->scope->print();

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder(context);
    llvm::Module module("L25", context);

    // 生成表达式并将其值打印出来
    llvm::Value* val = rootProgram->codeGen(builder, context, module);
    if (!val || hasError) {
        std::cerr << "LLVM IR生成失败，停止" << std::endl;
        return 1;
    }

    // 输出完整 IR 到标准输出
    module.print(llvm::outs(), nullptr);  // 输出完整 IR

    // 清理资源
    delete rootProgram;

    return 0;
}