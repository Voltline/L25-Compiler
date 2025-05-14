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
    // 声明printf函数，用于后续的输出
    if (!module.getFunction("printf")) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
            true // 可变参数
        );
        llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", module);
    }

    // 声明scanf函数，用于后续的输入
    if (!module.getFunction("scanf")) {
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
            true // 可变参数
        );
        llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage, "scanf", module);
    }

    // main函数
    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);
    llvm::Function* mainFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", module);

    // 入口基本块
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);

    // 生成main_body的IR
    main_body->codeGen(builder, context, module);

    // 添加默认返回
    builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
    return mainFunc;
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
    llvm::Value* last = nullptr;
    for (const auto& stmt: stmts) {
        last = stmt->codeGen(builder, context, module);
    }
    return last;
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
    llvm::Type* intType = llvm::Type::getInt32Ty(context);
    llvm::AllocaInst* alloca = nullptr;

    const std::string& ident_name = name->ident;
    auto typeInfo = name->type;

    if (typeInfo.kind == SymbolKind::Int) {
        // 普通整形的分配
        alloca = builder.CreateAlloca(intType, nullptr, ident_name);
    } else if (typeInfo.kind == SymbolKind::Array) {
        // 多维数组的分配
        llvm::Type* arrayType = intType;
        for (auto it{ typeInfo.dims.rbegin() }; it != typeInfo.dims.rend(); it++) {
            arrayType = llvm::ArrayType::get(arrayType, *it);
        }
        alloca = builder.CreateAlloca(arrayType, nullptr, ident_name);

        // 类型参数列表：i8* 和 i64
        auto memsetFn = llvm::Intrinsic::getDeclaration(
            &module,
            llvm::Intrinsic::memset,
            {
                llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
                llvm::Type::getInt64Ty(context)
            }
        );

        // 构造参数
        llvm::Value* zeroVal = llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), 0);
        llvm::Value* sizeVal = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(context),
            module.getDataLayout().getTypeAllocSize(arrayType)
        );
        llvm::Value* isVolatile = llvm::ConstantInt::getFalse(context);

        // 调用 memset
        builder.CreateCall(memsetFn, {
            builder.CreateBitCast(alloca, llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)),
            zeroVal,
            sizeVal,
            isVolatile
        });
    }

    if (!alloca) {
        std::cerr << "错误：无法为变量分配空间：" << ident_name << std::endl;
        return nullptr;
    }

    // 找到符号
    SymbolInfo* symbolInfo = scope->lookupLocal(ident_name);
    symbolInfo->addr = alloca;

    // 存在赋值
    if (expr) {
        llvm::Value* initVal = expr->codeGen(builder, context, module);
        if (initVal) {
            // 普通变量直接存入
            if (typeInfo.kind == SymbolKind::Int) {
                builder.CreateStore(initVal, alloca);
            }
            // TODO: 数组初始化还没做呢（目前只是零初始化）
        }
    }
    return alloca;
}

// 赋值语句节点
AssignStmt::AssignStmt(std::unique_ptr<Expr> name, std::unique_ptr<Expr> expr)
    : name(std::move(name)), expr(std::move(expr)) {}

void AssignStmt::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Assign" << std::endl;
    name->print(indent + 2);
    expr->print(indent + 2);
}

llvm::Value* AssignStmt::codeGen(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const  
{
    llvm::Value* rhs = expr->codeGen(builder, context, module);
    if (!rhs) {
        std::cerr << "错误：赋值右侧表达式生成失败" << std::endl;
        return nullptr;
    }

    llvm::Value* lhsAddr = nullptr;
    
    // 普通变量
    if (auto identExpr = dynamic_cast<IdentExpr*>(name.get())) {
        SymbolInfo* symbol = scope->lookup(identExpr->ident);
        if (!symbol || !symbol->addr) {
            std::cerr << "错误：变量未声明或未分配空间：" << identExpr->ident << std::endl;
            return nullptr;
        }
        lhsAddr = symbol->addr;
    } else if (auto arrayExpr = dynamic_cast<ArraySubscriptExpr*>(name.get())) {
        lhsAddr = arrayExpr->getAddress(builder, context, module);
        if (!lhsAddr) {
            std::cerr << "错误：获取数组元素地址失败" << std::endl;
            return nullptr;
        }
    } else {
        std::cerr << "错误：左值类型错误" << std::endl;
        return nullptr;
    }

    if (!lhsAddr) {

    }
    builder.CreateStore(rhs, lhsAddr);
    return rhs;
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
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    llvm::Value* condValue = condition->codeGen(builder, context, module);
    if (!condValue) {
        std::cerr << "错误：if条件表达式生成失败" << std::endl;
        return nullptr;
    }

    // 创建基本块
    llvm::BasicBlock* ifBody = llvm::BasicBlock::Create(context, "if.then", function);
    llvm::BasicBlock* elseBody = else_body ? llvm::BasicBlock::Create(context, "if.else") : nullptr;
    llvm::BasicBlock* merge = llvm::BasicBlock::Create(context, "if.end");

    // 创建条件跳转
    if (elseBody) {
        builder.CreateCondBr(condValue, ifBody, elseBody);
    } else {
        builder.CreateCondBr(condValue, ifBody, merge);
    }

    // if-body
    builder.SetInsertPoint(ifBody);
    if (if_body) {
        if_body->codeGen(builder, context, module);
    }
    if (!ifBody->getTerminator()) {
        builder.CreateBr(merge);
    }

    // else-body
    if (elseBody) {
        function->insert(function->end(), elseBody);
        builder.SetInsertPoint(elseBody);
        else_body->codeGen(builder, context, module);
        if (!elseBody->getTerminator()) {
            builder.CreateBr(merge);
        }
    }

    // merge block
    function->insert(function->end(), merge);
    builder.SetInsertPoint(merge);

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
    llvm::Function* scanfFunc = module.getFunction("scanf");
    if (!scanfFunc) {
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
            true
        );
        scanfFunc = llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage, "scanf", module);
    }

    llvm::Value* formatStr = builder.CreateGlobalStringPtr("%d");

    for (const auto& ident: idents) {
        assert(scope && "InputStmt::codeGen 中的 scope 为空");
        SymbolInfo* symbol = scope->lookup(ident->ident);
        if (!symbol) {
            std::cerr << "错误：变量 " << ident->ident << " 未声明" << std::endl;
            return nullptr;
        }

        if (symbol->kind == SymbolKind::Array) {
            std::cerr << "错误：不支持直接输入数组，已跳过 " << ident->ident << std::endl;
            continue;
        }

        if (!symbol->addr) {
            std::cerr << "错误：变量 " << ident->ident << " 未分配空间" << std::endl;
            return nullptr;
        }
        builder.CreateCall(scanfFunc, { formatStr, symbol->addr });
    }
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
    llvm::Function* printfFunc = module.getFunction("printf");
    if (!printfFunc) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
            true
        );
        printfFunc = llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", module);
    }

    // "%d\n"形式的字符串
    llvm::Value* formatStr = builder.CreateGlobalStringPtr("%d\n");

    for (const auto& expr: idents) {
        llvm::Value* val = expr->codeGen(builder, context, module);
        if (val) {
            builder.CreateCall(printfFunc, { formatStr, val });
        }
    }
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
    llvm::Value* lhsVal = lhs->codeGen(builder, context, module);
    llvm::Value* rhsVal = rhs->codeGen(builder, context, module);

    if (!lhsVal || !rhsVal) {
        std::cerr << "错误：布尔表达式左右子表达式生成失败" << std::endl;
        return nullptr;
    }

    if (lhsVal->getType()->isIntegerTy() && rhsVal->getType()->isIntegerTy()) {
        if (symbol == "==") {
            return builder.CreateICmpEQ(lhsVal, rhsVal, "cmpeq");
        } else if (symbol == "!=") {
            return builder.CreateICmpNE(lhsVal, rhsVal, "cmpne");
        } else if (symbol == "<") {
            return builder.CreateICmpSLT(lhsVal, rhsVal, "cmplt");
        } else if (symbol == "<=") {
            return builder.CreateICmpSLE(lhsVal, rhsVal, "cmple");
        } else if (symbol == ">") {
            return builder.CreateICmpSGT(lhsVal, rhsVal, "cmpgt");
        } else if (symbol == ">=") {
            return builder.CreateICmpSGE(lhsVal, rhsVal, "cmpge");
        } else {
            std::cerr << "错误：不支持的布尔操作符 " << symbol << std::endl;
            return nullptr;
        }
    } else {
        std::cerr << "错误：暂不支持非整数类型的布尔比较" << std::endl;
        return nullptr;
    }
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
    llvm::Value* LHS = lhs->codeGen(builder, context, module);
    llvm::Value* RHS = rhs->codeGen(builder, context, module);
    if (!LHS || !RHS) {
        std::cerr << "错误：二元运算的子表达式生成失败" << std::endl;
        return nullptr;
    }

    switch (op) {
    case '+':
        return builder.CreateAdd(LHS, RHS, "addtmp");
    case '-':
        return builder.CreateSub(LHS, RHS, "subtmp");
    case '*':
        return builder.CreateMul(LHS, RHS, "multmp");
    case '/':
        return builder.CreateSDiv(LHS, RHS, "divtmp");
    default:
        std::cerr << "错误：不支持的二元运算符 " << op << std::endl;
        return nullptr;
    }
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
    SymbolInfo* symbol = scope->lookup(array->ident);
    if (!symbol) {
        std::cerr << "错误：数组 " << array->ident << " 未声明" << std::endl;
        return nullptr;
    }

    llvm::AllocaInst* arrayPtr = symbol->addr;
    if (!arrayPtr) {
        std::cerr << "错误：数组 " << array->ident << " 未分配空间" << std::endl;
        return nullptr;
    }

    std::vector<llvm::Value*> indices;

    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));

    for (int i: subscript) {
        indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i));
    }

    llvm::Type* baseType = llvm::Type::getInt32Ty(context);

    // 只构建未被索引的那部分维度
    for (size_t i = symbol->dimensions.size(); i > 0; i--) {
        baseType = llvm::ArrayType::get(baseType, symbol->dimensions[i - 1]);
    }

    llvm::Value* gep = builder.CreateGEP(
        baseType,
        arrayPtr,
        indices,
        "array_elem"
    );

    llvm::Type* valueType = llvm::Type::getInt32Ty(context);
    return builder.CreateLoad(valueType, gep, "load_elem");
}

llvm::Value* ArraySubscriptExpr::getAddress(llvm::IRBuilder<>& builder, llvm::LLVMContext& context, llvm::Module& module) const {
    SymbolInfo* symbol = scope->lookup(array->ident);
    if (!symbol) {
        std::cerr << "错误：数组 " << array->ident << " 未声明" << std::endl;
        return nullptr;
    }

    llvm::AllocaInst* arrayPtr = symbol->addr;
    if (!arrayPtr) {
        std::cerr << "错误：数组 " << array->ident << " 未分配空间" << std::endl;
        return nullptr;
    }

    std::vector<llvm::Value*> indices;
    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
    for (int i : subscript) {
        indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i));
    }

    llvm::Type* baseArrayType = llvm::Type::getInt32Ty(context);
    for (auto it = symbol->dimensions.rbegin(); it != symbol->dimensions.rend(); it++) {
        baseArrayType = llvm::ArrayType::get(baseArrayType, *it);
    }

    llvm::Value* gep = builder.CreateGEP(
        baseArrayType,
        arrayPtr,
        indices,
        "array_elem"
    );

    return gep;
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
    SymbolInfo* symbol = scope->lookup(ident);

    if (!symbol) {
        std::cerr << "错误：标识符 " << ident << " 未找到" << std::endl;
        return nullptr;
    }

    // 如果为变量
    if (symbol->kind == SymbolKind::Int) {
        llvm::Type* valueType = llvm::Type::getInt32Ty(context);
        return builder.CreateLoad(valueType, symbol->addr, ident);
    }

    std::cerr << "错误：不支持返回的标识符 " << ident << std::endl;
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