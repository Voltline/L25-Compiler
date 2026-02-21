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

// 判断表达式是否产生拥有所有权的字符串缓冲区（无需深拷贝）
bool isOwnedStringExpr(const Expr* expr);

// ===== 容器运行时函数 =====
// 声明所有容器运行时函数 (l25_vector_*, l25_map_*)
void ensureContainerRuntimeDeclared(CodeGenContext& ctx);
// 计算 TypeInfo 对应的 LLVM 类型的字节大小
uint64_t getTypeAllocSize(const TypeInfo& typeInfo, CodeGenContext& ctx);
// 获取 map key type tag (L25_KEY_INT=0, L25_KEY_FLOAT=1, L25_KEY_STRING=2)
int32_t getMapKeyTypeTag(const TypeInfo& keyType);
// 从 TypeInfo 获取容器元素类型（vector → typeParams[0], map → typeParams[1] for value）
TypeInfo getContainerElemType(const SymbolInfo* symbol);
TypeInfo getContainerKeyType(const SymbolInfo* symbol);
TypeInfo getContainerValueType(const SymbolInfo* symbol);

// ===== GC 运行时函数 =====
// 声明所有 GC 运行时函数 (l25_gc_*)
void ensureGCRuntimeDeclared(CodeGenContext& ctx);
// 为指定类生成 GC 扫描函数 __gc_scan_ClassName
void emitGCScanFunction(CodeGenContext& ctx, const std::string& className);
