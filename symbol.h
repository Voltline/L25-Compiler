#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

enum class SymbolKind {
    Int, Array, Function, Program
};

struct SymbolInfo 
{
    SymbolKind kind;
    std::string name;                   // 对应ident节点的name
    std::vector<int> dimensions;        // 展平后的数组维度信息
    std::vector<SymbolKind> paramTypes; // 函数参数类型信息
    llvm::Value* value = nullptr;       // LLVM变量或函数指针
};

class SymbolTable 
{
public:
    // 加入符号表方法
    bool declare(const std::string& name, const SymbolInfo& info);

    // 查询符号表方法
    SymbolInfo* lookup(const std::string& name);
private:
    // 用哈希表存储符号表
    std::unordered_map<std::string, SymbolInfo> table;
};

class SymbolTableManager
{
public:
    // 进入下一层作用域方法
    void enterScope();
    // 离开当前作用域方法
    void exitScope();

    // 加入符号表方法(仅限当前作用域)
    bool declare(const std::string& name, const SymbolInfo& info);
    // 查询符号表方法(在当前及上层作用域)
    SymbolInfo* lookup(const std::string& name);
    // 同层查询符号表方法(仅在当前作用域)
    SymbolInfo* lookupInplace(const std::string& name);
private:
    // 分作用域符号表
    std::vector<SymbolTable> scopes;
};