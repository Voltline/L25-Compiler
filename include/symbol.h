#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

enum class SymbolKind {
    Int,
    Float,
    Array,
    Pointer,
    Class,
    Function,
    Program,
    Invalid
};

const char* const SymbolName[] {
    "Int", "Float", "Array", "Pointer", "Class", "Function", "Program", "Invalid"
};

struct TypeInfo
{
    SymbolKind kind; // 类型类别
    std::vector<int> dims; // 数组维度
    int pointerLevel; // 指针层级
    bool isFloat; // 是否为浮点类型
    std::string className; // 类名（仅用于类/类指针类型）

    TypeInfo();
    TypeInfo(SymbolKind kind, std::vector<int> dims, int pointerLevel = 0, bool isFloat = false, std::string className = "");
};
struct Func;

struct SymbolInfo
{
    SymbolKind kind;
    std::string name;                   // 对应ident节点的name
    std::vector<int> dimensions;        // 展平后的数组维度信息
    std::vector<TypeInfo> paramTypes;   // 函数参数类型信息
    const Func* funcDef = nullptr;      // 函数定义指针（用于闭包捕获）
    llvm::Value* value = nullptr;       // LLVM变量或函数指针
    llvm::Value* addr = nullptr;        // LLVM变量栈地址
    bool isFuncParam = false;           // 是否为函数参数(影响数组访问)
    std::string llvmName = "";               // LLVM中的唯一名字
    int pointerLevel = 0;               // 指针层级
    bool isFloat = false;               // 浮点标识
    std::string className;              // 类名（用于类及类指针类型）
    TypeInfo returnType;                // 函数/方法返回类型
    bool hasDestructor = false;         // 类是否定义析构函数
    // 类专用信息
    std::vector<std::pair<std::string, TypeInfo>> classFields; // 记录字段布局
    std::unordered_map<std::string, std::vector<TypeInfo>> methodParamTypes; // 方法参数类型
    std::unordered_map<std::string, TypeInfo> methodReturnTypes; // 方法返回类型
    
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

    void remove(const std::string& name);

    void print(int depth = 0) const;
private:
    std::unordered_map<std::string, SymbolInfo> table;
    std::vector<std::unique_ptr<Scope>> children;
    Scope* parent;
};