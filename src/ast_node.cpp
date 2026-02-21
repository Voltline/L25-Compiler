#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

// ===== TypeInfo 构造函数 =====
TypeInfo::TypeInfo()
    : kind(SymbolKind::Invalid), dims(), pointerLevel(0), isFloat(false), className("") {}

TypeInfo::TypeInfo(SymbolKind kind, std::vector<int> dims, int pointerLevel, bool isFloat, std::string className)
    : kind(kind)
    , dims(std::move(dims))
    , pointerLevel(pointerLevel)
    , isFloat(isFloat)
    , className(std::move(className)) {}

// ===== ASTNode =====
void ASTNode::reportError(const std::string& msg) const
{
    reportErrorAt(*this, "代码生成", msg);
}

// ===== 程序节点 =====
Program::Program(std::unique_ptr<IdentExpr> name,
                 std::vector<std::unique_ptr<ClassDecl>> classes,
                 std::vector<std::unique_ptr<Func>> functions,
                 std::unique_ptr<StmtList> main_body)
    : name(std::move(name))
    , classes(std::move(classes))
    , functions(std::move(functions))
    , main_body(std::move(main_body)) {}

void Program::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Program(" << *name << ")" << std::endl;
    for (const auto& cls : classes) {
        cls->print(indent + 2);
    }
    for (const auto& func: functions) {
        func->print(indent + 2);
    }
    main_body->print(indent + 2);
}

llvm::Value* Program::codeGen(CodeGenContext& ctx) const  
{
    // 声明printf函数，用于后续的输出
    if (!ctx.module.getFunction("printf")) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true // 可变参数
        );
        llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", ctx.module);
    }

    // 声明scanf函数，用于后续的输入
    if (!ctx.module.getFunction("scanf")) {
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true // 可变参数
        );
        llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage, "scanf", ctx.module);
    }

    // 声明字符串运行时辅助函数
    ensureStringRuntimeDeclared(ctx);

    // 类定义（目前仅占位）
    for (const auto& cls : classes) {
        cls->codeGen(ctx);
    }

    // 函数部分
    for (auto& function: functions) {
        function->codeGen(ctx);
    }

    // main函数
    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx.context), false);
    llvm::Function* mainFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", ctx.module);

    // 入口基本块
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx.context, "entry", mainFunc);
    ctx.builder.SetInsertPoint(entry);

    // 生成main_body的IR
    main_body->codeGen(ctx);

    // 添加默认返回
    ctx.builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
    return mainFunc;
}

// ===== 语句列表节点 =====
StmtList::StmtList(std::vector<std::unique_ptr<Stmt>> stmts)
    : stmts(std::move(stmts)) {}

void StmtList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "StmtList" << std::endl;
    for (const auto& stmt: stmts) {
        stmt->print(indent + 2);
    }
}

llvm::Value* StmtList::codeGen(CodeGenContext& ctx) const 
{
    llvm::Value* last = nullptr;

    ctx.currentBlock = ctx.builder.GetInsertBlock(); // 初始插入点

    for (auto& stmt : stmts) {
        if (ctx.currentBlock->getTerminator()) {
            // 该块已终结，不再插入后续语句
            break;
        }

        ctx.builder.SetInsertPoint(ctx.currentBlock); // 确保插入点正确

        if (auto* funcDefStmt = dynamic_cast<Func*>(stmt.get())) {
            auto* oldInsertPoint = ctx.builder.GetInsertBlock();
            last = funcDefStmt->codeGen(ctx);
            ctx.builder.SetInsertPoint(oldInsertPoint); // 恢复现场
        } else {
            last = stmt->codeGen(ctx);
        }

        // 更新 currentBlock（避免插入到死块）
        ctx.currentBlock = ctx.builder.GetInsertBlock();
    }

    return last;
}

// ===== 参数列表节点 =====
ArgList::ArgList(std::vector<std::unique_ptr<Expr>> args)
    : args(std::move(args)) {}

void ArgList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "ArgList" << std::endl;
    for (const auto& arg: args) {
        arg->print(indent + 2);
    }
}

llvm::Value* ArgList::codeGen(CodeGenContext& ctx) const 
{
    return nullptr;
}

// ===== 形式参数列表节点 =====
ParamList::ParamList(std::vector<std::unique_ptr<IdentExpr>> params)
    : params(std::move(params)) {}

void ParamList::print(int indent) const 
{
    std::cout << std::string(indent, ' ') << "ParamList" << std::endl;
    for (const auto& param: params) {
        param->print(indent + 2);
    }
}

llvm::Value* ParamList::codeGen(CodeGenContext& ctx) const 
{
    return nullptr;
}

// ===== 输入函数参数列表节点 =====
InputArgList::InputArgList(std::vector<std::unique_ptr<Expr>> idents)
    : idents(std::move(idents)) {}

void InputArgList::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Input Args" << std::endl;
    for (const auto& ident: idents) {
        ident->print(indent + 2);
    }
}

llvm::Value* InputArgList::codeGen(CodeGenContext& ctx) const
{
    return nullptr;
}
