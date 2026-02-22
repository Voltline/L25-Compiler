#pragma once
#include "symbol.h"
#include <memory>
#include <string>
#include <iostream>
#include <unordered_map>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

struct ASTNode;
struct Program;
struct Func;
struct ClassDecl;
struct FieldDecl;
struct MethodDecl;
struct CtorDecl;
struct MemberAccessExpr;
struct MethodCallExpr;
struct NewExpr;
struct NewArrayExpr;
struct Stmt;
struct StmtList;
struct DeclareStmt;
struct AssignStmt;
struct IfStmt;
struct WhileStmt;
struct ForStmt;
struct FuncCallStmt;
struct InputStmt;
struct OutputStmt;
struct Expr;
struct BoolExpr;
struct NumberExpr;
struct FloatNumberExpr;
struct UnaryExpr;
struct BinaryExpr;
struct IdentExpr;
struct FuncCallExpr;
struct StringLiteralExpr;
struct StrlenExpr;
struct TypenameExpr;
struct FieldCountExpr;
struct MethodCountExpr;
struct FieldNameExpr;
struct MethodNameExpr;
struct InvokeExpr;
struct ArgList;
struct ParamList;
struct InputArgList;
struct TypeInfo;
enum class SymbolKind;
class Scope;

// ===== RAII 清理类型 =====
enum class CleanupKind {
    String,      // 释放 string.data
    ClassPtr,    // GC 模式下仅移除根
    Vector,      // l25_vector_destroy
    Map,         // l25_map_destroy
    GCRoot,      // GC 根注销（this 指针 / 函数参数）
};

struct CleanupEntry {
    llvm::Value* addr;       // 变量的 alloca 地址
    CleanupKind kind;
    std::string className;   // 仅用于 ClassPtr（析构函数查找）
};

struct CodeGenContext
{
    llvm::LLVMContext& context;
    llvm::Module& module;
    llvm::IRBuilder<>& builder;
    llvm::Function* currentFunction;
    llvm::BasicBlock* currentBlock = nullptr;

    // RAII 清理栈：每个作用域一个 CleanupEntry 列表
    std::vector<std::vector<CleanupEntry>> cleanupStack;

    void pushCleanupScope() { cleanupStack.emplace_back(); }
    void popCleanupScope()  { if (!cleanupStack.empty()) cleanupStack.pop_back(); }
    void registerCleanup(llvm::Value* addr, CleanupKind kind,
                         const std::string& className = "") {
        if (!cleanupStack.empty()) {
            cleanupStack.back().push_back({addr, kind, className});
        }
    }

    // 构造函数简化传参
    CodeGenContext(llvm::LLVMContext& ctx,
                   llvm::Module& mod,
                   llvm::IRBuilder<>& b)
        : context(ctx), module(mod), builder(b) {}
};

extern std::unordered_map<std::string, llvm::StructType*> classStructTypes;
extern std::unordered_map<std::string, std::vector<std::pair<std::string, TypeInfo>>> classFieldLayouts;
extern std::unordered_map<std::string, std::unordered_map<std::string, TypeInfo>> classMethodReturnTypes;
extern std::unordered_map<std::string, std::vector<std::string>> classMethodNames;
TypeInfo evaluateExprType(const Expr* expr);

// AST节点基类
struct ASTNode
{
    virtual ~ASTNode() = default;

    virtual void print(int indent = 0) const = 0;
    // 必须实现codeGen
    virtual llvm::Value* codeGen(CodeGenContext& ctx) const = 0;

    // 当前节点对应的作用域树节点
    Scope* scope = nullptr;
    int lineno = 0;
    int column = 0;

    void reportError(const std::string& msg) const;
};

// 程序节点
struct Program: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    std::vector<std::unique_ptr<ClassDecl>> classes;
    std::vector<std::unique_ptr<Func>> functions;
    std::unique_ptr<StmtList> main_body;

    Program(std::unique_ptr<IdentExpr> name,
            std::vector<std::unique_ptr<ClassDecl>> classes,
            std::vector<std::unique_ptr<Func>> functions,
            std::unique_ptr<StmtList> main_body);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 语句节点
struct Stmt: public ASTNode {};

// 类字段声明
struct FieldDecl: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    TypeInfo type;

    FieldDecl(std::unique_ptr<IdentExpr> name, TypeInfo type);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 构造函数声明
struct CtorDecl: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<ParamList> params;
    std::unique_ptr<StmtList> body;
    Scope* bodyScope = nullptr;

    CtorDecl(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> body);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 析构函数声明
struct DtorDecl: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<StmtList> body;
    Scope* bodyScope = nullptr;

    DtorDecl(std::unique_ptr<IdentExpr> name, std::unique_ptr<StmtList> body);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 方法声明
struct MethodDecl: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<ParamList> params;
    std::unique_ptr<StmtList> body;
    std::unique_ptr<Expr> return_value;
    TypeInfo returnType;
    Scope* bodyScope = nullptr;

    MethodDecl(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> body, std::unique_ptr<Expr> return_value, TypeInfo returnType = TypeInfo{ SymbolKind::Int, {} });

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 类声明
struct ClassDecl: public ASTNode
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<IdentExpr> baseClass; // Nullable
    std::vector<std::unique_ptr<FieldDecl>> fields;
    std::vector<std::unique_ptr<MethodDecl>> methods;
    std::vector<std::unique_ptr<CtorDecl>> ctors;
    std::unique_ptr<DtorDecl> dtor;

    ClassDecl(std::unique_ptr<IdentExpr> name,
              std::unique_ptr<IdentExpr> baseClass,
              std::vector<std::unique_ptr<FieldDecl>> fields,
              std::vector<std::unique_ptr<MethodDecl>> methods,
              std::vector<std::unique_ptr<CtorDecl>> ctors,
              std::unique_ptr<DtorDecl> dtor = nullptr);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 函数定义节点
struct Func: public Stmt
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<ParamList> params; // Nullable
    std::unique_ptr<StmtList> stmts;
    std::unique_ptr<Expr> return_value;
    TypeInfo returnType;
    Scope* body_scope;
    std::vector<SymbolInfo*> captures; // 闭包捕获列表

    Func(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> stmts, std::unique_ptr<Expr> return_value, TypeInfo returnType = TypeInfo{ SymbolKind::Int, {} });

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 语句列表节点
struct StmtList: public ASTNode
{
    std::vector<std::unique_ptr<Stmt>> stmts;

    StmtList(std::vector<std::unique_ptr<Stmt>> stmts);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override ;
};

// 声明语句节点
struct DeclareStmt: public Stmt
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<Expr> expr; // Nullable

    DeclareStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<Expr> expr);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 赋值语句节点
struct AssignStmt: public Stmt
{
    std::unique_ptr<Expr> name;
    std::unique_ptr<Expr> expr;

    AssignStmt(std::unique_ptr<Expr> name, std::unique_ptr<Expr> expr);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 条件分支语句节点
struct IfStmt: public Stmt {
    std::unique_ptr<BoolExpr> condition;
    std::unique_ptr<StmtList> if_body;
    std::unique_ptr<StmtList> else_body; // Nullable
    Scope* ifScope;
    Scope* elseScope;

    IfStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> if_body, std::unique_ptr<StmtList> else_body);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// While循环语句节点
struct WhileStmt: public Stmt
{
    std::unique_ptr<BoolExpr> condition;
    std::unique_ptr<StmtList> loop_body;
    Scope* loopBodyScope;

    WhileStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> loop_body);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// For循环语句节点
struct ForStmt: public Stmt
{
    std::unique_ptr<Stmt> init;           // 初始化语句 (declare / assign)
    std::unique_ptr<BoolExpr> condition;  // 循环条件
    std::unique_ptr<Stmt> step;           // 步进语句 (assign)
    std::unique_ptr<StmtList> loop_body;
    Scope* loopBodyScope;

    ForStmt(std::unique_ptr<Stmt> init, std::unique_ptr<BoolExpr> condition,
            std::unique_ptr<Stmt> step, std::unique_ptr<StmtList> loop_body);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 函数调用语句节点
struct FuncCallStmt: public Stmt
{
    std::unique_ptr<IdentExpr> name;
    std::unique_ptr<ArgList> args; // Nullable

    FuncCallStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<ArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 表达式语句（将任意 Expr 包装为 Stmt，用于 invoke 等可以作为独立语句使用的表达式）
struct ExprStmt: public Stmt
{
    std::unique_ptr<Expr> expr;
    explicit ExprStmt(std::unique_ptr<Expr> expr) : expr(std::move(expr)) {}
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 输入语句节点
struct InputStmt: public Stmt
{
    std::vector<std::unique_ptr<Expr>> idents;

    InputStmt(std::vector<std::unique_ptr<Expr>> idents);

    InputStmt(std::unique_ptr<InputArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 输出语句节点
struct OutputStmt: public Stmt
{
    std::vector<std::unique_ptr<Expr>> idents;

    OutputStmt(std::vector<std::unique_ptr<Expr>> idents);

    OutputStmt(std::unique_ptr<ArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// delete 语句（已弃用：RAII 自动管理资源，保留结构体用于兼容）
struct DeleteStmt: public Stmt
{
    std::unique_ptr<Expr> target;

    explicit DeleteStmt(std::unique_ptr<Expr> target);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 表达式节点
struct Expr: public ASTNode {};

// Bool表达式节点
struct BoolExpr: public ASTNode
{
    std::string symbol;  // 比较: "==","!=","<","<=",">",">="  逻辑: "&&","||","!"
    // 比较运算时使用
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
    // 逻辑运算时使用（&&, ||, !）
    std::unique_ptr<BoolExpr> bool_lhs;
    std::unique_ptr<BoolExpr> bool_rhs;

    // 比较运算构造函数
    BoolExpr(std::string symbol, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs);
    // 逻辑二元运算构造函数 (&&, ||)
    BoolExpr(std::string symbol, std::unique_ptr<BoolExpr> bool_lhs, std::unique_ptr<BoolExpr> bool_rhs);
    // 逻辑一元运算构造函数 (!)
    BoolExpr(std::string symbol, std::unique_ptr<BoolExpr> operand);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 整数常量节点
struct NumberExpr: public Expr
{
    int value;
    NumberExpr(int val);
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

struct FloatNumberExpr: public Expr
{
    double value;
    explicit FloatNumberExpr(double val);
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

struct NilExpr: public Expr
{
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 一元运算符节点
struct UnaryExpr: public Expr
{
    char op;
    std::unique_ptr<Expr> rhs;
    UnaryExpr(char op, std::unique_ptr<Expr> rhs);
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 取地址表达式节点
struct AddressOfExpr: public Expr
{
    std::unique_ptr<Expr> target;
    AddressOfExpr(std::unique_ptr<Expr> target);
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 解引用表达式节点
struct DereferenceExpr: public Expr
{
    std::unique_ptr<Expr> pointerExpr;
    DereferenceExpr(std::unique_ptr<Expr> pointerExpr);
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
    llvm::Value* getPointerValue(CodeGenContext& ctx) const;
};

// 二元运算符节点
struct BinaryExpr: public Expr 
{
    char op;
    std::unique_ptr<Expr> lhs, rhs;
    BinaryExpr(char op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 数组下标访问运算节点
struct ArraySubscriptExpr: public Expr
{
    std::unique_ptr<IdentExpr> array;
    std::vector<std::unique_ptr<Expr>> subscript;
    ArraySubscriptExpr(std::unique_ptr<IdentExpr> array, std::vector<std::unique_ptr<Expr>> subscript);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
    llvm::Value* getAddress(CodeGenContext& ctx) const;
};

// 成员访问表达式
struct MemberAccessExpr: public Expr
{
    std::unique_ptr<Expr> target;
    std::unique_ptr<IdentExpr> member;

    MemberAccessExpr(std::unique_ptr<Expr> target, std::unique_ptr<IdentExpr> member);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
    llvm::Value* getPointer(CodeGenContext& ctx) const;
};

// 方法调用表达式
struct MethodCallExpr: public Expr
{
    std::unique_ptr<Expr> target;
    std::unique_ptr<IdentExpr> method;
    std::unique_ptr<ArgList> args; // Nullable

    MethodCallExpr(std::unique_ptr<Expr> target, std::unique_ptr<IdentExpr> method, std::unique_ptr<ArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// new 表达式
struct NewExpr: public Expr
{
    std::unique_ptr<IdentExpr> className;
    std::unique_ptr<ArgList> args; // Nullable

    NewExpr(std::unique_ptr<IdentExpr> className, std::unique_ptr<ArgList> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// new T[n] 数组堆分配表达式
struct NewArrayExpr: public Expr
{
    std::string elementTypeName; // "int" 或 "float"
    bool isFloat;
    std::unique_ptr<Expr> sizeExpr;

    NewArrayExpr(const std::string& elementTypeName, bool isFloat, std::unique_ptr<Expr> sizeExpr);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 标识符节点
struct IdentExpr: public Expr
{
    std::string ident;
    TypeInfo type;
    IdentExpr(const std::string& ident, TypeInfo type = TypeInfo());

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;

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

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};
 
// 参数列表节点
struct ArgList: public ASTNode
{
    std::vector<std::unique_ptr<Expr>> args;

    ArgList(std::vector<std::unique_ptr<Expr>> args);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 形式参数列表节点
struct ParamList: public ASTNode
{
    std::vector<std::unique_ptr<IdentExpr>> params;

    ParamList(std::vector<std::unique_ptr<IdentExpr>> params);
    
    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 输入函数参数列表节点
/* 为什么要设计这个节点？
 * Input在原始定义下可以与正常函数共用ParamList
 * 但我扩展的语法中会出现指针和数组类型，这一块在ParamList里会体现，但Input不允许
 * 因此使用一个只包含Ident的节点来表示输入函数专用的参数列表
 */
struct InputArgList: public ASTNode
{
    std::vector<std::unique_ptr<Expr>> idents;

    InputArgList(std::vector<std::unique_ptr<Expr>> idents);

    void print(int indent = 0) const override;

    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

extern Program* rootProgram;

// 字符串字面量节点
struct StringLiteralExpr: public Expr
{
    std::string value;
    explicit StringLiteralExpr(const std::string& val);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// strlen 内建函数节点
struct StrlenExpr: public Expr
{
    std::unique_ptr<Expr> target;
    explicit StrlenExpr(std::unique_ptr<Expr> target);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// ===== 反射内建函数节点 =====

// typename(expr) -> 返回类名字符串
struct TypenameExpr: public Expr
{
    std::unique_ptr<Expr> target;
    explicit TypenameExpr(std::unique_ptr<Expr> target);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// fieldcount(expr) -> 返回字段数量
struct FieldCountExpr: public Expr
{
    std::unique_ptr<Expr> target;
    explicit FieldCountExpr(std::unique_ptr<Expr> target);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// methodcount(expr) -> 返回方法数量
struct MethodCountExpr: public Expr
{
    std::unique_ptr<Expr> target;
    explicit MethodCountExpr(std::unique_ptr<Expr> target);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// fieldname(expr, index) -> 返回第 index 个字段的名称
struct FieldNameExpr: public Expr
{
    std::unique_ptr<Expr> target;
    std::unique_ptr<Expr> index;
    FieldNameExpr(std::unique_ptr<Expr> target, std::unique_ptr<Expr> index);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// methodname(expr, index) -> 返回第 index 个方法的名称
struct MethodNameExpr: public Expr
{
    std::unique_ptr<Expr> target;
    std::unique_ptr<Expr> index;
    MethodNameExpr(std::unique_ptr<Expr> target, std::unique_ptr<Expr> index);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// invoke(obj, name_expr [, args...]) -> 运行时按名称调用方法
struct InvokeExpr: public Expr
{
    std::unique_ptr<Expr> target;
    std::unique_ptr<Expr> methodName;
    std::unique_ptr<ArgList> args;  // nullable
    InvokeExpr(std::unique_ptr<Expr> target, std::unique_ptr<Expr> methodName, std::unique_ptr<ArgList> args);
    void print(int indent = 0) const override;
    llvm::Value* codeGen(CodeGenContext& ctx) const override;
};

// 获取或创建 L25 字符串结构体类型 %__l25_string = type { i32, i8* }
llvm::StructType* getL25StringType(llvm::LLVMContext& ctx);