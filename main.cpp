#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <filesystem>

#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Bitcode/BitcodeWriter.h>

#include "include/ast.h"
#include "include/errorReporter.h"
#include "include/semanticAnalysis.h"
#include "parser.tab.hpp"

extern int yyparse();
extern FILE* yyin;  // Bison/Flex 使用的输入流
Program* rootProgram = nullptr;
bool hasError = false;

std::string findCommand(const std::vector<std::string>& candidates)
{
    for (const auto& name: candidates) {
        std::string check = "command -v " + name + " >/dev/null 2>&1";
        if (std::system(check.c_str()) == 0) {
            return name;
        }
    }
    return {};
}

int main(int argc, const char* argv[]) 
{
    std::string inputFile;
    std::string outputFile = "a.out";
    bool emitAST = false;
    bool emitScope = false;
    bool emitIR = false;
    bool emitBC = false;

    // 先扫描是否要显示帮助
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-help" || arg == "--help") {
            std::cout << "用法: l25cc <source.l25> [options]\n"
                      << "选项:\n"
                      << "  -emit-ast      输出 AST\n"
                      << "  -emit-scope    输出作用域信息\n"
                      << "  -emit-ir       输出 LLVM IR (.ll)\n"
                      << "  -emit-bc       输出 LLVM Bitcode (.bc)\n"
                      << "  -o <file>      指定输出文件名\n"
                      << "  -help          显示此帮助信息\n";
            return 0;
        }
    }

    // 参数解析
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];

        if (arg == "-emit-ast") emitAST = true;
        else if (arg == "-emit-scope") emitScope = true;
        else if (arg == "-emit-ir") emitIR = true;
        else if (arg == "-emit-bc") emitBC = true;
        else if (arg == "-o" && i + 1 < argc) {
            outputFile = argv[++i];
        }
        else if (inputFile.empty() && std::filesystem::exists(arg)) {
            inputFile = arg;
        }
        else {
            std::cerr << "未知参数或无效文件: " << arg << "\n";
            return 1;
        }
    }

    if (inputFile.empty()) {
        std::cerr << "未指定源代码文件，请使用 --help 查看用法\n";
        return 1;
    }

    // 打开源文件
    FILE* input = fopen(inputFile.c_str(), "r");
    if (!input) {
        std::cerr << "无法打开输入文件: " << inputFile << "\n";
        return 1;
    }
    if (!loadSourceFile(inputFile)) {
        fclose(input);
        return 1;
    }
    yyin = input;

    // 开始解析
    yyparse();
    fclose(input);

    if (!rootProgram || hasError) {
        std::cerr << "语法解析失败，停止\n";
        return 1;
    }

    if (emitAST)
        rootProgram->print();

    SemanticAnalyzer analyzer;
    analyzer.analyze(*rootProgram);

    if (hasError) {
        std::cerr << "语义分析失败，停止\n";
        return 1;
    }

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder(context);

    // 使用输入文件名作为 module 名
    llvm::Module module(std::filesystem::path(inputFile).filename().string(), context);
    // 修改Triple信息以适应平台
    module.setTargetTriple(llvm::sys::getDefaultTargetTriple());
    CodeGenContext ctx{ context, module, builder };

    llvm::Value* val = rootProgram->codeGen(ctx);
    if (!val || hasError) {
        std::cerr << "IR生成失败，停止\n";
        return 1;
    }

    if (emitScope && rootProgram->scope)
        rootProgram->scope->print();

    if (emitIR || emitBC || outputFile.ends_with(".ll") || outputFile.ends_with(".bc")) {
        std::error_code EC;
        llvm::raw_fd_ostream out(outputFile, EC, llvm::sys::fs::OF_None);
        if (EC) {
            std::cerr << "无法写入输出文件: " << EC.message() << "\n";
            return 1;
        }

        if (emitBC || outputFile.ends_with(".bc")) {
            llvm::WriteBitcodeToFile(module, out);
        } else {
            module.print(out, nullptr);  // 输出 .ll 格式
        }
        out.flush();
    } else {
        // 如果用户希望得到可执行文件
        std::string llFile = "tmp.ll";
        std::string bcFile = "tmp.bc";

        {
            std::error_code EC;
            llvm::raw_fd_ostream out(llFile, EC, llvm::sys::fs::OF_None);
            module.print(out, nullptr);
            out.flush();
        }

        std::string llvmAs = findCommand({"llvm-as", "llvm-as-18"});
        std::string clangBin = findCommand({"clang", "clang-18"});

        if (llvmAs.empty() || clangBin.empty()) {
            std::cerr << "未找到可用的 llvm-as 或 clang，请确认已安装 LLVM 工具链\n";
            return 1;
        }

        std::string cmd = llvmAs + " " + llFile + " -o " + bcFile + " && " + clangBin + " " + bcFile + " -o " + outputFile;
        int ret = system(cmd.c_str());
        if (ret != 0) {
            std::cerr << "链接失败: llvm-as 或 clang 报错\n";
            return 1;
        }

        std::filesystem::remove(llFile);
        std::filesystem::remove(bcFile);
    }


    delete rootProgram;
    return 0;
}