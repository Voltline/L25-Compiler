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
