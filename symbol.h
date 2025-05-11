#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

struct TypeInfo;
struct Func;

enum class SymbolKind {
    Int, Array, Function, Program, Invalid
};

const char* const SymbolName[] {
    "Int", "Array", "Function", "Program", "Invalid" 
};

struct SymbolInfo 
{
    SymbolKind kind;
    std::string name;                   // 对应ident节点的name
    std::vector<int> dimensions;        // 展平后的数组维度信息
    std::vector<TypeInfo> paramTypes; // 函数参数类型信息
    llvm::Value* value = nullptr;       // LLVM变量或函数指针

    SymbolInfo(SymbolKind kind, const std::string& name);
    SymbolInfo(const std::string& name, const TypeInfo& type);
    SymbolInfo(const std::string& name, const Func& func);
};

// 作用域
class Scope
{
public:
    Scope(Scope* parent = nullptr);
    // 本地及上层
    SymbolInfo* lookup(const std::string& name);
    // 本地
    SymbolInfo* lookupLocal(const std::string& name);
    bool declare(const std::string& name, const SymbolInfo& info);

    Scope* createChild();
    Scope* getParent() const;

    void print(int depth = 0) const;
private:
    std::unordered_map<std::string, SymbolInfo> table;
    std::vector<std::unique_ptr<Scope>> children;
    Scope* parent;
};