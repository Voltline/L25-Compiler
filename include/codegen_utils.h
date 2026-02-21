#pragma once
#include "ast.h"
#include <unordered_map>

// ===== 全局映射表（定义在 codegen_utils.cpp）=====
extern std::unordered_map<std::string, int> functionMap;

// ===== 代码生成工具函数 =====
llvm::Type* buildArrayType(llvm::Type* elementType, const std::vector<int>& dims);
llvm::Type* typeInfoToLLVMType(const TypeInfo& typeInfo, llvm::LLVMContext& ctx, bool decayArrayToPointer);
llvm::Type* typeInfoToLLVMValueType(const TypeInfo& typeInfo, llvm::LLVMContext& ctx);
llvm::Value* castValueToType(llvm::Value* value, llvm::Type* targetType, CodeGenContext& ctx);
llvm::Value* defaultValueForType(const TypeInfo& typeInfo, CodeGenContext& ctx);
std::string buildCtorName(const std::string& className, size_t paramCount);
std::string buildDtorName(const std::string& className);
TypeInfo typeInfoFromSymbol(const SymbolInfo* symbol);
void ensureStringRuntimeDeclared(CodeGenContext& ctx);

// ===== RAII 清理函数 =====
// 为单个清理条目生成释放 IR
void emitCleanupForEntry(CodeGenContext& ctx, const CleanupEntry& entry);
// 生成当前作用域的清理代码并弹出
void emitScopeCleanup(CodeGenContext& ctx);
// 为所有活跃作用域生成清理代码（用于 return 前）
void emitReturnCleanup(CodeGenContext& ctx);
// 字符串深拷贝：返回拥有独立 malloc 缓冲区的新 string struct
llvm::Value* emitStringDeepCopy(llvm::Value* strVal, CodeGenContext& ctx);
// 释放字符串变量的旧数据（赋值前调用）
void emitStringFree(llvm::Value* strAddr, CodeGenContext& ctx);
// 释放旧类指针并置空（赋值前调用）
void emitClassPtrFree(llvm::Value* ptrAddr, const std::string& className, CodeGenContext& ctx);
