#include "include/ast.h"

// TypeInfo 方法
TypeInfo::TypeInfo()
    : kind(SymbolKind::Invalid), dims() {}

TypeInfo::TypeInfo(SymbolKind kind, std::vector<int> dims)
    : kind(kind), dims(std::move(dims)) {}

// 程序节点
Program::Program(std::unique_ptr<IdentExpr> name, std::vector<std::unique_ptr<Func>> functions, std::unique_ptr<StmtList> main_body)
    : name(std::move(name))
    , functions(std::move(functions))
    , main_body(std::move(main_body)) {}

void Program::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Program(" << *name << ")" << std::endl;
    for (const auto& func: functions) {
        func->print(indent + 2);
    }
    main_body->print(indent + 2);
}

llvm::Value* Program::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 函数节点
Func::Func(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> stmts, std::unique_ptr<Expr> return_value)
    : name(std::move(name))
    , params(std::move(params))
    , stmts(std::move(stmts))
    , return_value(std::move(return_value)) {}

void Func::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Func" << std::endl;
    name->print(indent + 2);
    if (params) {
        params->print(indent + 2);
    }
    stmts->print(indent + 2);
    return_value->print(indent + 2);
}

llvm::Value* Func::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 语句列表节点
StmtList::StmtList(std::vector<std::unique_ptr<Stmt>> stmts)
    : stmts(std::move(stmts)) {}

void StmtList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "StmtList" << std::endl;
    for (const auto& stmt: stmts) {
        stmt->print(indent + 2);
    }
}

llvm::Value* StmtList::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 声明语句节点
DeclareStmt::DeclareStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<Expr> expr)
    : name(std::move(name)), expr(std::move(expr)) {}

void DeclareStmt::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Declare" << std::endl;
    name->print(indent + 2);
    if (expr) {
        expr->print(indent + 2);
    }
}

llvm::Value* DeclareStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 赋值语句节点
AssignStmt::AssignStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<Expr> expr)
    : name(std::move(name)), expr(std::move(expr)) {}

void AssignStmt::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Assign" << std::endl;
    name->print(indent + 2);
    expr->print(indent + 2);
}

llvm::Value* AssignStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 条件分支语句节点
IfStmt::IfStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> if_body, std::unique_ptr<StmtList> else_body)
    : condition(std::move(condition))
    , if_body(std::move(if_body))
    , else_body(std::move(else_body)) {}

void IfStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "if" << std::endl;
    condition->print(indent + 2);
    if_body->print(indent + 2);
    if (else_body) {
        else_body->print(indent + 2);
    }
}

llvm::Value* IfStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// While循环语句节点
WhileStmt::WhileStmt(std::unique_ptr<BoolExpr> condition, std::unique_ptr<StmtList> loop_body)
    : condition(std::move(condition))
    , loop_body(std::move(loop_body)) {}

void WhileStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "while" << std::endl;
    condition->print(indent + 2);
    loop_body->print(indent + 2);
}

llvm::Value* WhileStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 函数调用语句节点
FuncCallStmt::FuncCallStmt(std::unique_ptr<IdentExpr> name, std::unique_ptr<ArgList> args)
    : name(std::move(name)), args(std::move(args)) {}

void FuncCallStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Call" << std::endl;
    name->print(indent + 2);
    if (args) {
        args->print(indent + 2);
    }
}

llvm::Value* FuncCallStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 输入语句节点
InputStmt::InputStmt(std::vector<std::unique_ptr<IdentExpr>> idents)
    : idents(std::move(idents)) {}

InputStmt::InputStmt(std::unique_ptr<InputArgList> args)
    : idents(std::move(args->idents)) {}

void InputStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Input" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* InputStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 输出语句节点
OutputStmt::OutputStmt(std::vector<std::unique_ptr<Expr>> idents)
    : idents(std::move(idents)) {}

OutputStmt::OutputStmt(std::unique_ptr<ArgList> args)
    : idents(std::move(args->args)) {}

void OutputStmt::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Output" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* OutputStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// Bool表达式节点
BoolExpr::BoolExpr(std::string symbol, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
    : symbol(symbol)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs)) {}

void BoolExpr::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Bool(" << symbol << ")" << std::endl;
    lhs->print(indent + 2);
    rhs->print(indent + 2);
}

llvm::Value* BoolExpr::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 整数常量节点
NumberExpr::NumberExpr(int val) : value(val) {}

void NumberExpr::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Number(" << value << ")" << std::endl;
}

llvm::Value* NumberExpr::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const 
{
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), value);
}

// 一元运算符节点
UnaryExpr::UnaryExpr(char op, std::unique_ptr<Expr> rhs): op(op), rhs(std::move(rhs)) {}

void UnaryExpr::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Unary(" << op << ")" << std::endl;
    rhs->print(indent+2);
}

llvm::Value* UnaryExpr::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const 
{
    llvm::Value* RHS = rhs->codeGen(builder, context, module);
    switch (op) {
    case '+':
        return RHS;
    case '-':
        return builder.CreateNeg(RHS);
    default:
        std::cerr << "暂未实现" << std::endl;
        return nullptr;
    }
}

// 二元运算符节点
BinaryExpr::BinaryExpr(char op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

void BinaryExpr::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Binary(" << op << ")" << std::endl;
    lhs->print(indent + 2);
    rhs->print(indent + 2);
}

llvm::Value* BinaryExpr::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const 
{
    // TODO: 符号表未实现，暂不实现codeGen
    std::cerr << "暂未实现BinaryExpr的codeGen" << std::endl;
    return nullptr;
    // llvm::Value* LHS = lhs->codeGen(builder, context, module);
    // llvm::Value* RHS = rhs->codeGen(builder, context, module);
    // if (!LHS || !RHS) return nullptr;

    // switch (op) {
    // case '+':
    //     return builder.CreateAdd(LHS, RHS, "addtmp");
    // case '-':
    //     return builder.CreateSub(LHS, RHS, "addtmp");
    // case '*':
    //     return builder.CreateMul(LHS, RHS, "addtmp");
    // case '/':
    //     return builder.CreateSDiv(LHS, RHS, "addtmp");
    // default:
    //     return nullptr;
    // }
}

// 数组下标访问运算节点
ArraySubscriptExpr::ArraySubscriptExpr(std::unique_ptr<IdentExpr> array, std::vector<int> subscript)
    : array(std::move(array)), subscript(std::move(subscript)) {}

void ArraySubscriptExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Array Subscript(" << array->ident << "[";
    for (int i = 0; i < subscript.size(); i++) {
        if (i != 0) std::cout << ",";
        std::cout << subscript[i];
    }
    std::cout << "])" << std::endl;
}

llvm::Value* ArraySubscriptExpr::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 标识符节点
IdentExpr::IdentExpr(const std::string& ident, TypeInfo type) : ident(ident), type(type) {}

void IdentExpr::print(int indent) const 
{
    switch (type.kind) {
    case SymbolKind::Int:
    case SymbolKind::Function:
    case SymbolKind::Program:
        std::cout << std::string(indent, ' ') 
            << "Ident(" << ident << ": " 
            << SymbolName[static_cast<int>(type.kind)] << ")" << std::endl;
        break;
    case SymbolKind::Array:
        std::cout << std::string(indent, ' ') 
            << "Ident(" << ident << ": Array[";
        for (int i = 0; i < type.dims.size(); i++) {
            if (i != 0) std::cout << ",";
            std::cout << type.dims[i];
        }
        std::cout << "])" << std::endl;
        break;
    default:
        std::cout << std::string(indent, ' ') 
            << "Ident(" << ident << ")" << std::endl;
    }
}

llvm::Value* IdentExpr::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const 
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 函数调用表达式节点
FuncCallExpr::FuncCallExpr(std::unique_ptr<IdentExpr> name, std::unique_ptr<ArgList> args)
    : name(std::move(name)), args(std::move(args)) {}

FuncCallExpr::FuncCallExpr(std::unique_ptr<FuncCallStmt> funcCallStmt) 
    : name(std::move(funcCallStmt->name)), args(std::move(funcCallStmt->args)) {}

void FuncCallExpr::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "Call" << std::endl;
    name->print(indent + 2);
    if (args) {
        args->print(indent + 2);
    }
}

llvm::Value* FuncCallExpr::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 参数列表节点
ArgList::ArgList(std::vector<std::unique_ptr<Expr>> args)
    : args(std::move(args)) {}

void ArgList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "ArgList" << std::endl;
    for (const auto& arg: args) {
        arg->print(indent + 2);
    }
}

llvm::Value* ArgList::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const 
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 形式参数列表节点
ParamList::ParamList(std::vector<std::unique_ptr<IdentExpr>> params)
    : params(std::move(params)) {}

void ParamList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "ParamList" << std::endl;
    for (const auto& param: params) {
        param->print(indent + 2);
    }
}

llvm::Value* ParamList::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const 
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}

// 输入函数参数列表节点
InputArgList::InputArgList(std::vector<std::unique_ptr<IdentExpr>> idents)
    : idents(std::move(idents)) {}

void InputArgList::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Input Args" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* InputArgList::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const
{
    std::cerr << "暂未实现" << std::endl;
    return nullptr;
}