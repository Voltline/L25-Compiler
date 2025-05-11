#pragma once
#include "symbol.h"
#include <memory>
#include <string>
#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

struct ASTNode;
struct Program;
struct Func;
struct Stmt;
struct StmtList;
struct DeclareStmt;
struct AssignStmt;
struct IfStmt;
struct WhileStmt;
struct FuncCallStmt;
struct InputStmt;
struct OutputStmt;
struct Expr;
struct BoolExpr;
struct NumberExpr;
struct UnaryExpr;
struct BinaryExpr;
struct IdentExpr;
struct FuncCallExpr;
struct ArgList;
struct ParamList;
struct InputArgList;
struct TypeInfo;
enum class SymbolKind;
class Scope;

// 类型参数，不是AST节点
struct TypeInfo
{
    SymbolKind kind; // 默认不使用
    std::vector<int> dims; // 数组维度

    TypeInfo();
    TypeInfo(SymbolKind kind, std::vector<int> dims);
};

// AST节点基类
struct ASTNode
{
    virtual ~ASTNode() = default;

    virtual void print(int indent = 0) const = 0;
    // 必须实现codeGen
    virtual llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const = 0;

    // 当前节点对应的作用域树节点
    Scope* scope; 
};

// 程序节点
struct Program: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    std::vector<std::unique_ptr<Func>> functions;
    std::unique_ptr<StmtList> main_body;

    Program(std::unique_ptr<IdentExpr> name, std::vector<std::unique_ptr<Func>> functions, std::unique_ptr<StmtList> main_body);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 函数节点
struct Func: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<ParamList> params; // Nullable
    std::unique_ptr<StmtList> stmts;
    std::unique_ptr<Expr> return_value;

    Func(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> stmts, std::unique_ptr<Expr> return_value);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 语句节点
struct Stmt: public ASTNode {};

// 语句列表节点
struct StmtList: public ASTNode
{
    std::vector<std::unique_ptr<Stmt>> stmts;

    StmtList(std::vector<std::unique_ptr<Stmt>> stmts);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override ;
};

// 声明语句节点
struct DeclareStmt: public Stmt
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<Expr> expr; // Nullable

    DeclareStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<Expr> expr);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 赋值语句节点
struct AssignStmt: public Stmt
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<Expr> expr;

    AssignStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<Expr> expr);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 条件分支语句节点
struct IfStmt: public Stmt {
    std::unique_ptr<BoolExpr> condition;
    std::unique_ptr<StmtList> if_body;
    std::unique_ptr<StmtList> else_body; // Nullable

    IfStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> if_body, std::unique_ptr<StmtList> else_body);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// While循环语句节点
struct WhileStmt: public Stmt
{
    std::unique_ptr<BoolExpr> condition;
    std::unique_ptr<StmtList> loop_body;

    WhileStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> loop_body);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 函数调用语句节点
struct FuncCallStmt: public Stmt
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<ArgList> args; // Nullable

    FuncCallStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<ArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 输入语句节点
struct InputStmt: public Stmt
{
    std::vector<std::unique_ptr<IdentExpr>> idents;

    InputStmt(std::vector<std::unique_ptr<IdentExpr>> idents);

    InputStmt(std::unique_ptr<InputArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 输出语句节点
struct OutputStmt: public Stmt
{
    std::vector<std::unique_ptr<Expr>> idents;

    OutputStmt(std::vector<std::unique_ptr<Expr>> idents);

    OutputStmt(std::unique_ptr<ArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 表达式节点
struct Expr: public ASTNode {};

// Bool表达式节点
struct BoolExpr: public ASTNode
{
    std::string symbol;
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;

    BoolExpr(std::string symbol, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 整数常量节点
struct NumberExpr: public Expr 
{
    int value;
    NumberExpr(int val);
    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 一元运算符节点
struct UnaryExpr: public Expr
{
    char op;
    std::unique_ptr<Expr> rhs;
    UnaryExpr(char op, std::unique_ptr<Expr> rhs);
    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 二元运算符节点
struct BinaryExpr: public Expr 
{
    char op;
    std::unique_ptr<Expr> lhs, rhs;
    BinaryExpr(char op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 标识符节点
struct IdentExpr: public Expr
{
    std::string ident;
    TypeInfo type;
    IdentExpr(const std::string& ident, TypeInfo type = TypeInfo());

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;

    friend std::ostream& operator<<(std::ostream& os, const IdentExpr& ident) {
        os << ident.ident;
        return os;
    }
};

// 函数调用表达式节点
struct FuncCallExpr: public Expr
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<ArgList> args; // Nullable

    FuncCallExpr(std::unique_ptr<IdentExpr> name, std::unique_ptr<ArgList> args);

    FuncCallExpr(std::unique_ptr<FuncCallStmt> funcCallStmt);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};
 
// 参数列表节点
struct ArgList: public ASTNode
{
    std::vector<std::unique_ptr<Expr>> args;

    ArgList(std::vector<std::unique_ptr<Expr>> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 形式参数列表节点
struct ParamList: public ASTNode
{
    std::vector<std::unique_ptr<IdentExpr>> params;

    ParamList(std::vector<std::unique_ptr<IdentExpr>> params);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

// 输入函数参数列表节点
/* 为什么要设计这个节点？
 * Input在原始定义下可以与正常函数共用ParamList
 * 但我扩展的语法中会出现指针和数组类型，这一块在ParamList里会体现，但Input不允许
 * 因此使用一个只包含Ident的节点来表示输入函数专用的参数列表
 */
struct InputArgList: public ASTNode
{
    std::vector<std::unique_ptr<IdentExpr>> idents;

    InputArgList(std::vector<std::unique_ptr<IdentExpr>> idents);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override;
};

extern Program* rootProgram;