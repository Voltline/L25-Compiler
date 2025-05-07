#pragma once
#include <memory>
#include <string>
#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

// 表达式节点
struct Expr
{
    virtual ~Expr() = default;
    virtual void print(int indent = 0) const = 0;

    virtual llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const = 0;
};

extern Expr* rootExpr;

// 整数常量
struct NumberExpr: Expr 
{
    int value;
    NumberExpr(int val) : value(val) {}
    void print(int indent = 0) const override 
    {
        std::cout << std::string(indent, ' ') << "Number(" << value << ")" << std::endl;
    }

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override
    {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), value);
    }
};

// 二元运算符
struct BinaryExpr: Expr 
{
    char op;
    std::unique_ptr<Expr> lhs, rhs;
    BinaryExpr(char op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
        : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    void print(int indent = 0) const override
    {
        std::cout << std::string(indent, ' ') << "Binary(" << op << ")" << std::endl;
        lhs->print(indent + 2);
        rhs->print(indent + 2);
    }

    llvm::Value* codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const override
    {
        llvm::Value* LHS = lhs->codeGen(builder, context, module);
        llvm::Value* RHS = rhs->codeGen(builder, context, module);
        if (!LHS || !RHS) return nullptr;

        switch (op) {
        case '+':
            return builder.CreateAdd(LHS, RHS, "addtmp");
        case '-':
            return builder.CreateSub(LHS, RHS, "addtmp");
        case '*':
            return builder.CreateMul(LHS, RHS, "addtmp");
        case '/':
            return builder.CreateSDiv(LHS, RHS, "addtmp");
        default:
            return nullptr;
        }
    }
};