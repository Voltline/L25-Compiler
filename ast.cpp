#include "include/ast.h"
#include "include/errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <unordered_map>
#include <algorithm>
extern bool hasError;
std::unordered_map<std::string, int> functionMap;
std::unordered_map<std::string, llvm::StructType*> classStructTypes;
std::unordered_map<std::string, std::vector<std::pair<std::string, TypeInfo>>> classFieldLayouts;
static std::string currentClassNameCodegen;

static llvm::Type* wrapPointer(llvm::Type* base, int pointerLevel)
{
    for (int i = 0; i < pointerLevel; ++i) {
        base = llvm::PointerType::get(base, 0);
    }
    return base;
}

static llvm::Type* buildArrayType(llvm::Type* elementType, const std::vector<int>& dims)
{
    llvm::Type* arrayType = elementType;
    for (auto it = dims.rbegin(); it != dims.rend(); ++it) {
        arrayType = llvm::ArrayType::get(arrayType, *it);
    }
    return arrayType;
}

static llvm::Type* typeInfoToLLVMType(const TypeInfo& typeInfo, llvm::LLVMContext& ctx, bool decayArrayToPointer)
{
    llvm::Type* baseType = nullptr;
    llvm::Type* scalarType = (typeInfo.kind == SymbolKind::Float || typeInfo.isFloat)
        ? llvm::Type::getFloatTy(ctx)
        : llvm::Type::getInt32Ty(ctx);

    if (typeInfo.kind == SymbolKind::Int || typeInfo.kind == SymbolKind::Float) {
        baseType = scalarType;
    } else if (typeInfo.kind == SymbolKind::Array) {
        auto* arrayType = buildArrayType(scalarType, typeInfo.dims);
        baseType = decayArrayToPointer ? llvm::PointerType::get(arrayType, 0) : arrayType;
    } else if (typeInfo.kind == SymbolKind::Pointer) {
        baseType = scalarType;
    } else if (typeInfo.kind == SymbolKind::Class) {
        auto it = classStructTypes.find(typeInfo.className);
        if (it != classStructTypes.end()) {
            baseType = it->second;
        }
    }

    if (!baseType) return nullptr;

    int wrapTimes = typeInfo.pointerLevel;
    if (typeInfo.kind == SymbolKind::Array && decayArrayToPointer && wrapTimes > 0) {
        // 数组已经退化为指针，额外的指针层级需要在此基础上继续包裹
        wrapTimes -= 1;
    }
    return wrapPointer(baseType, wrapTimes);
}

static llvm::Type* typeInfoToLLVMValueType(const TypeInfo& typeInfo, llvm::LLVMContext& ctx)
{
    llvm::Type* scalarType = (typeInfo.kind == SymbolKind::Float || typeInfo.isFloat)
        ? llvm::Type::getFloatTy(ctx)
        : llvm::Type::getInt32Ty(ctx);

    if (typeInfo.kind == SymbolKind::Pointer) {
        return wrapPointer(scalarType, std::max(1, typeInfo.pointerLevel));
    }
    return wrapPointer(
        typeInfo.kind == SymbolKind::Array
            ? buildArrayType(scalarType, typeInfo.dims)
            : scalarType,
        typeInfo.pointerLevel
    );
}

static llvm::Value* castValueToType(llvm::Value* value, llvm::Type* targetType, CodeGenContext& ctx)
{
    if (!value || !targetType) return value;
    llvm::Type* srcType = value->getType();
    if (srcType == targetType) return value;

    if (targetType->isFloatTy()) {
        if (srcType->isIntegerTy()) {
            return ctx.builder.CreateSIToFP(value, targetType, "sitofp");
        }
        if (srcType->isFloatTy()) {
            return ctx.builder.CreateFPCast(value, targetType, "fpc");
        }
    }

    if (targetType->isIntegerTy()) {
        if (srcType->isFloatTy()) {
            return ctx.builder.CreateFPToSI(value, targetType, "fptosi");
        }
        if (srcType->isIntegerTy()) {
            return ctx.builder.CreateIntCast(value, targetType, true, "intcast");
        }
    }

    if (targetType->isPointerTy() && srcType->isPointerTy()) {
        return ctx.builder.CreateBitCast(value, targetType, "bitcast");
    }

    if (targetType->isPointerTy() && srcType->isIntegerTy()) {
        return ctx.builder.CreateIntToPtr(value, targetType, "inttoptr");
    }

    return value;
}

static TypeInfo typeInfoFromSymbol(const SymbolInfo* symbol)
{
    if (!symbol) return TypeInfo{};
    return TypeInfo{ symbol->kind, symbol->dimensions, symbol->pointerLevel, symbol->isFloat, symbol->className };
}

TypeInfo evaluateExprType(const Expr* expr)
{
    if (!expr) return TypeInfo{ SymbolKind::Invalid, {}, 0 };
    if (dynamic_cast<const NumberExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Int, {}, 0, false };
    }
    if (dynamic_cast<const FloatNumberExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Float, {}, 0, true };
    }
    if (auto ident = dynamic_cast<const IdentExpr*>(expr)) {
        SymbolInfo* symbol = ident->scope ? ident->scope->lookup(ident->ident) : nullptr;
        if (symbol) return typeInfoFromSymbol(symbol);
        return TypeInfo{ SymbolKind::Int, {}, 0 };
    }
    if (auto arrayExpr = dynamic_cast<const ArraySubscriptExpr*>(expr)) {
        Scope* lookupScope = arrayExpr->scope ? arrayExpr->scope : (arrayExpr->array ? arrayExpr->array->scope : nullptr);
        SymbolInfo* symbol = lookupScope ? lookupScope->lookup(arrayExpr->array->ident) : nullptr;
        bool isFloatElem = symbol && symbol->isFloat;
        return TypeInfo{ isFloatElem ? SymbolKind::Float : SymbolKind::Int, {}, 0, isFloatElem };
    }
    if (auto addrExpr = dynamic_cast<const AddressOfExpr*>(expr)) {
        TypeInfo baseType = evaluateExprType(addrExpr->target.get());
        if (baseType.kind == SymbolKind::Array) {
            baseType.kind = SymbolKind::Pointer;
            baseType.dims.clear();
        } else if (baseType.kind == SymbolKind::Invalid) {
            baseType.kind = SymbolKind::Pointer;
        }
        baseType.pointerLevel += 1;
        if (baseType.kind == SymbolKind::Int || baseType.kind == SymbolKind::Float) {
            baseType.kind = SymbolKind::Pointer;
        }
        return baseType;
    }
    if (auto derefExpr = dynamic_cast<const DereferenceExpr*>(expr)) {
        TypeInfo baseType = evaluateExprType(derefExpr->pointerExpr.get());
        if (baseType.pointerLevel > 0) {
            baseType.pointerLevel -= 1;
            if (baseType.pointerLevel == 0 && baseType.kind == SymbolKind::Pointer) {
                baseType.kind = baseType.isFloat ? SymbolKind::Float : SymbolKind::Int;
            }
        } else {
            baseType.kind = baseType.isFloat ? SymbolKind::Float : SymbolKind::Int;
        }
        return baseType;
    }
    if (auto binary = dynamic_cast<const BinaryExpr*>(expr)) {
        TypeInfo lhsType = evaluateExprType(binary->lhs.get());
        TypeInfo rhsType = evaluateExprType(binary->rhs.get());
        bool isFloatResult = lhsType.isFloat || rhsType.isFloat || lhsType.kind == SymbolKind::Float || rhsType.kind == SymbolKind::Float;
        if (isFloatResult) {
            return TypeInfo{ SymbolKind::Float, {}, 0, true };
        }
        return TypeInfo{ SymbolKind::Int, {}, 0 };
    }
    if (auto unary = dynamic_cast<const UnaryExpr*>(expr)) {
        TypeInfo rhsType = evaluateExprType(unary->rhs.get());
        if (rhsType.isFloat || rhsType.kind == SymbolKind::Float) {
            return TypeInfo{ SymbolKind::Float, {}, rhsType.pointerLevel, true };
        }
        return TypeInfo{ SymbolKind::Int, {}, rhsType.pointerLevel };
    }
    if (auto member = dynamic_cast<const MemberAccessExpr*>(expr)) {
        TypeInfo targetType = evaluateExprType(member->target.get());
        auto layoutIt = classFieldLayouts.find(targetType.className);
        if (layoutIt != classFieldLayouts.end()) {
            auto fit = std::find_if(layoutIt->second.begin(), layoutIt->second.end(), [&](const auto& f){return f.first == member->member->ident;});
            if (fit != layoutIt->second.end()) {
                return fit->second;
            }
        }
        return TypeInfo{ SymbolKind::Invalid, {}, 0 };
    }
    if (dynamic_cast<const MethodCallExpr*>(expr)) {
        return TypeInfo{ SymbolKind::Int, {}, 0 };
    }
    return TypeInfo{ SymbolKind::Int, {}, 0 };
}

// TypeInfo 方法
TypeInfo::TypeInfo()
    : kind(SymbolKind::Invalid), dims(), pointerLevel(0), isFloat(false), className("") {}

TypeInfo::TypeInfo(SymbolKind kind, std::vector<int> dims, int pointerLevel, bool isFloat, std::string className)
    : kind(kind)
    , dims(std::move(dims))
    , pointerLevel(pointerLevel)
    , isFloat(isFloat)
    , className(std::move(className)) {}

// ASTNode 方法
void ASTNode::reportError(const std::string& msg) const
{
    reportErrorAt(*this, "代码生成", msg);
}

// 程序节点
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

// 类字段
FieldDecl::FieldDecl(std::unique_ptr<IdentExpr> name, TypeInfo type)
    : name(std::move(name))
    , type(std::move(type)) {}

void FieldDecl::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "FieldDecl" << std::endl;
    name->print(indent + 2);
}

llvm::Value* FieldDecl::codeGen(CodeGenContext& ctx) const
{
    reportError("暂未实现类字段的代码生成");
    return nullptr;
}

// 构造函数
CtorDecl::CtorDecl(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> body)
    : name(std::move(name))
    , params(std::move(params))
    , body(std::move(body)) {}

void CtorDecl::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "CtorDecl" << std::endl;
    name->print(indent + 2);
    if (params) params->print(indent + 2);
    if (body) body->print(indent + 2);
}

llvm::Value* CtorDecl::codeGen(CodeGenContext& ctx) const
{
    reportError("暂未实现构造函数的代码生成");
    return nullptr;
}

// 方法
MethodDecl::MethodDecl(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> body, std::unique_ptr<Expr> return_value)
    : name(std::move(name))
    , params(std::move(params))
    , body(std::move(body))
    , return_value(std::move(return_value))
    , bodyScope(nullptr) {}

void MethodDecl::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "MethodDecl" << std::endl;
    name->print(indent + 2);
    if (params) params->print(indent + 2);
    if (body) body->print(indent + 2);
    if (return_value) return_value->print(indent + 2);
}

llvm::Value* MethodDecl::codeGen(CodeGenContext& ctx) const
{
    if (currentClassNameCodegen.empty()) {
        reportError("方法未关联类");
        return nullptr;
    }

    llvm::StructType* classTy = classStructTypes[currentClassNameCodegen];
    if (!classTy) {
        reportError("找不到类类型：" + currentClassNameCodegen);
        return nullptr;
    }

    std::vector<llvm::Type*> argTypes;
    argTypes.push_back(llvm::PointerType::get(classTy, 0));
    if (params) {
        for (const auto& param : params->params) {
            llvm::Type* argType = typeInfoToLLVMType(param->type, ctx.context, true);
            argTypes.push_back(argType);
        }
    }

    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx.context), argTypes, false);
    std::string funcName = currentClassNameCodegen + "." + name->ident;
    llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcName, ctx.module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx.context, "entry", function);
    ctx.builder.SetInsertPoint(entry);

    int idx = 0;
    for (auto& arg : function->args()) {
        if (idx == 0) {
            arg.setName("this");
            SymbolInfo* thisInfo = bodyScope ? bodyScope->lookupLocal("this") : nullptr;
            if (thisInfo) {
                llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(arg.getType(), nullptr, "this.addr");
                ctx.builder.CreateStore(&arg, alloca);
                thisInfo->addr = alloca;
                thisInfo->value = &arg;
            }
        } else {
            arg.setName(params->params[idx - 1]->ident);
            SymbolInfo* argInfo = bodyScope ? bodyScope->lookupLocal(arg.getName().str()) : nullptr;
            if (argInfo) {
                llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
                ctx.builder.CreateStore(&arg, alloca);
                argInfo->addr = alloca;
                argInfo->value = &arg;
            }
        }
        idx++;
    }

    if (body) {
        for (const auto& stmt : body->stmts) {
            stmt->codeGen(ctx);
        }
    }

    llvm::Value* retVal = return_value ? return_value->codeGen(ctx) : llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    retVal = castValueToType(retVal, llvm::Type::getInt32Ty(ctx.context), ctx);
    ctx.builder.CreateRet(retVal);
    return function;
}

// 类声明
ClassDecl::ClassDecl(std::unique_ptr<IdentExpr> name,
                     std::unique_ptr<IdentExpr> baseClass,
                     std::vector<std::unique_ptr<FieldDecl>> fields,
                     std::vector<std::unique_ptr<MethodDecl>> methods,
                     std::vector<std::unique_ptr<CtorDecl>> ctors)
    : name(std::move(name))
    , baseClass(std::move(baseClass))
    , fields(std::move(fields))
    , methods(std::move(methods))
    , ctors(std::move(ctors)) {}

void ClassDecl::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "ClassDecl(" << *name << ")" << std::endl;
    if (baseClass) {
        std::cout << std::string(indent + 2, ' ') << "extends ";
        baseClass->print(0);
    }
    for (const auto& field : fields) {
        field->print(indent + 2);
    }
    for (const auto& ctor : ctors) {
        ctor->print(indent + 2);
    }
    for (const auto& method : methods) {
        method->print(indent + 2);
    }
}

llvm::Value* ClassDecl::codeGen(CodeGenContext& ctx) const
{
    std::vector<llvm::Type*> fieldTypes;
    std::vector<std::pair<std::string, TypeInfo>> layout;
    for (const auto& field : fields) {
        llvm::Type* fieldType = typeInfoToLLVMType(field->type, ctx.context, true);
        if (!fieldType) {
            reportError("无法为字段生成类型：" + field->name->ident);
            continue;
        }
        fieldTypes.push_back(fieldType);
        layout.emplace_back(field->name->ident, field->type);
    }
    llvm::StructType* structTy = llvm::StructType::create(ctx.context, name->ident);
    structTy->setBody(fieldTypes, false);
    classStructTypes[name->ident] = structTy;
    classFieldLayouts[name->ident] = layout;

    std::string saved = currentClassNameCodegen;
    currentClassNameCodegen = name->ident;
    for (const auto& method : methods) {
        method->codeGen(ctx);
    }
    currentClassNameCodegen = saved;
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

llvm::Value* Func::codeGen(CodeGenContext& ctx) const  
{
    const std::string funcName= name->ident;

    SymbolInfo* funcSymbol = scope->lookup(funcName);
    if (!funcSymbol) {
        reportError("函数符号 " + funcName + " 不存在");
        return nullptr;
    }

    std::vector<llvm::Type*> argTypes;
    for (const auto& typeInfo: funcSymbol->paramTypes) {
        llvm::Type* argType = nullptr;
        if (typeInfo.kind == SymbolKind::Array) {
            argType = typeInfoToLLVMType(typeInfo, ctx.context, true);
        } else {
            argType = typeInfoToLLVMValueType(typeInfo, ctx.context);
        }
        argTypes.push_back(argType);
    }

    // 隐式捕获参数（以指针方式传递）
    for (auto* captured : captures) {
        if (!captured) continue;
        if (captured->kind == SymbolKind::Array) {
            TypeInfo captureType{ SymbolKind::Array, captured->dimensions, 1, captured->isFloat };
            argTypes.push_back(typeInfoToLLVMType(captureType, ctx.context, true));
        } else {
            TypeInfo captureType{ captured->kind, {}, captured->pointerLevel + 1, captured->isFloat };
            argTypes.push_back(typeInfoToLLVMValueType(captureType, ctx.context));
        }
    }
    
    if (!functionMap.contains(funcName)) {
        functionMap[funcName] = 1;
        funcSymbol->llvmName = funcName;
    } else {
        std::string tmpName = funcName + "." + std::to_string(functionMap[funcName]);
        funcSymbol->llvmName = tmpName;
        functionMap[funcName]++;
    }

    llvm::FunctionType* funcType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(ctx.context), argTypes, false
    );

    std::string funcLLVMName = funcSymbol->llvmName;
    llvm::Function* function = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, funcLLVMName, ctx.module
    );

    funcSymbol->value = function; // 更新函数指针

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx.context, "entry", function);
    ctx.builder.SetInsertPoint(entry);

    std::vector<std::pair<SymbolInfo*, llvm::Value*>> capturedOriginalAddrs;

    int idx = 0;
    for (auto& arg: function->args()) {
        if (params && idx < static_cast<int>(params->params.size())) {
            arg.setName(params->params[idx]->ident); // 设置形参名
            SymbolInfo* argInfo = body_scope->lookupLocal(params->params[idx]->ident);

            if (argInfo->kind == SymbolKind::Array) {
                argInfo->isFuncParam = true;
                auto* placeholder = ctx.builder.CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
                ctx.builder.CreateStore(&arg, placeholder);
                argInfo->addr = placeholder;
            } else {
                llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
                ctx.builder.CreateStore(&arg, alloca);
                argInfo->addr = alloca;
            }
            argInfo->value = &arg;
        } else {
            int captureIndex = idx - (params ? static_cast<int>(params->params.size()) : 0);
            if (captureIndex >= 0 && captureIndex < static_cast<int>(captures.size())) {
                SymbolInfo* capturedInfo = captures[captureIndex];
                std::string captureName = capturedInfo ? capturedInfo->name + "_capture" : "capture";
                arg.setName(captureName);
                if (capturedInfo) {
                    capturedOriginalAddrs.emplace_back(capturedInfo, capturedInfo->addr);
                    capturedInfo->addr = &arg;
                    capturedInfo->value = &arg;
                }
            }
        }
        idx++;
    }

    for (const auto& stmt: stmts->stmts) {
        if (dynamic_cast<const Func*>(stmt.get())) {
            llvm::IRBuilder<>::InsertPoint savedIP = ctx.builder.saveIP();
            stmt->codeGen(ctx);
            ctx.builder.restoreIP(savedIP);
        } else {
            stmt->codeGen(ctx);
        }
    }

    llvm::Value* retVal = return_value->codeGen(ctx);
    retVal = castValueToType(retVal, llvm::Type::getInt32Ty(ctx.context), ctx);
    ctx.builder.CreateRet(retVal);

    // 生成完毕后恢复捕获符号的原始地址
    for (auto& [symbol, originalAddr] : capturedOriginalAddrs) {
        symbol->addr = originalAddr;
    }

    return function;
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

llvm::Value* DeclareStmt::codeGen(CodeGenContext& ctx) const
{
    const std::string& ident_name = name->ident;
    auto typeInfo = name->type;
    llvm::Type* valueType = typeInfoToLLVMValueType(typeInfo, ctx.context);
    llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(valueType, nullptr, ident_name);

    if ((typeInfo.kind == SymbolKind::Int || typeInfo.kind == SymbolKind::Float) && typeInfo.pointerLevel == 0) {
        if (!expr) {
            llvm::Value* zeroInit = typeInfo.kind == SymbolKind::Float
                ? static_cast<llvm::Value*>(llvm::ConstantFP::get(valueType, 0.0))
                : static_cast<llvm::Value*>(llvm::ConstantInt::get(valueType, 0));
            ctx.builder.CreateStore(zeroInit, alloca);
        }
    } else if (typeInfo.kind == SymbolKind::Array && typeInfo.pointerLevel == 0) {
        // 类型参数列表：i8* 和 i64
        auto memsetFn = llvm::Intrinsic::getDeclaration(
            &ctx.module,
            llvm::Intrinsic::memset,
            {
                llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
                llvm::Type::getInt64Ty(ctx.context)
            }
        );

        llvm::Value* zeroVal = llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx.context), 0);
        llvm::Value* sizeVal = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(ctx.context),
            ctx.module.getDataLayout().getTypeAllocSize(valueType)
        );
        llvm::Value* isVolatile = llvm::ConstantInt::getFalse(ctx.context);

        ctx.builder.CreateCall(memsetFn, {
            ctx.builder.CreateBitCast(alloca, llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0)),
            zeroVal,
            sizeVal,
            isVolatile
        });
    } else if (valueType->isPointerTy() && !expr) {
        ctx.builder.CreateStore(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(valueType)), alloca);
    }

    if (!alloca) {
        reportError("无法为变量: " + ident_name + " 分配空间");
        return nullptr;
    }

    // 找到符号
    SymbolInfo* symbolInfo = scope->lookupLocal(ident_name);
    symbolInfo->addr = alloca;

    // 存在赋值
    if (expr) {
        llvm::Value* initVal = expr->codeGen(ctx);
        if (initVal) {
            llvm::Type* targetType = valueType;
            llvm::Value* stored = castValueToType(initVal, targetType, ctx);
            ctx.builder.CreateStore(stored, alloca);
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

llvm::Value* AssignStmt::codeGen(CodeGenContext& ctx) const  
{
    llvm::Value* rhs = expr->codeGen(ctx);
    if (!rhs) {
        reportError("赋值右侧表达式生成失败");
        return nullptr;
    }

    llvm::Value* lhsAddr = nullptr;
    
    SymbolInfo* targetSymbol = nullptr;

    if (auto identExpr = dynamic_cast<IdentExpr*>(name.get())) {
        SymbolInfo* symbol = scope->lookup(identExpr->ident);
        if (!symbol || !symbol->addr) {
            reportError("变量未声明或未分配空间: " + identExpr->ident);
            return nullptr;
        }
        lhsAddr = symbol->addr;
        targetSymbol = symbol;
    } else if (auto arrayExpr = dynamic_cast<ArraySubscriptExpr*>(name.get())) {
        lhsAddr = arrayExpr->getAddress(ctx);
        if (arrayExpr->array && arrayExpr->array->scope) {
            targetSymbol = arrayExpr->array->scope->lookup(arrayExpr->array->ident);
        }
        if (!lhsAddr) {
            reportError("获取数组元素地址失败");
            return nullptr;
        }
    } else if (auto memberExpr = dynamic_cast<MemberAccessExpr*>(name.get())) {
        lhsAddr = memberExpr->getPointer(ctx);
        if (!lhsAddr) {
            reportError("获取成员地址失败");
            return nullptr;
        }
    } else if (auto derefExpr = dynamic_cast<DereferenceExpr*>(name.get())) {
        lhsAddr = derefExpr->getPointerValue(ctx);
        if (!lhsAddr || !lhsAddr->getType()->isPointerTy()) {
            reportError("解引用目标不是合法的指针地址");
            return nullptr;
        }
    } else {
        reportError("左值类型错误");
        return nullptr;
    }

    TypeInfo lhsType = targetSymbol ? typeInfoFromSymbol(targetSymbol) : evaluateExprType(name.get());
    llvm::Type* targetType = typeInfoToLLVMValueType(lhsType, ctx.context);
    llvm::Value* storedValue = castValueToType(rhs, targetType, ctx);
    ctx.builder.CreateStore(storedValue, lhsAddr);
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

llvm::Value* IfStmt::codeGen(CodeGenContext& ctx) const  
{
    llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

    llvm::Value* condValue = condition->codeGen(ctx);
    if (!condValue) {
        reportError("if条件表达式生成失败");
        return nullptr;
    }

    // 创建块
    llvm::BasicBlock* ifBody = llvm::BasicBlock::Create(ctx.context, "if.then", function);
    llvm::BasicBlock* elseBody = else_body ? llvm::BasicBlock::Create(ctx.context, "if.else", function) : nullptr;
    llvm::BasicBlock* merge = llvm::BasicBlock::Create(ctx.context, "if.end", function);

    // 条件跳转
    if (elseBody) {
        ctx.builder.CreateCondBr(condValue, ifBody, elseBody);
    } else {
        ctx.builder.CreateCondBr(condValue, ifBody, merge);
    }

    // if-body
    ctx.builder.SetInsertPoint(ifBody);
    ctx.currentBlock = ifBody;
    if_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        ctx.builder.CreateBr(merge);
    }

    // else-body
    if (elseBody) {
        ctx.builder.SetInsertPoint(elseBody);
        ctx.currentBlock = elseBody;
        else_body->codeGen(ctx);

        if (!ctx.currentBlock->getTerminator()) {
            ctx.builder.SetInsertPoint(ctx.currentBlock);
            ctx.builder.CreateBr(merge);
        }
    }

    // 合并块
    ctx.builder.SetInsertPoint(merge);
    ctx.currentBlock = merge;

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

llvm::Value* WhileStmt::codeGen(CodeGenContext& ctx) const 
{
    llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

    // 创建基本块
    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(ctx.context, "while.cond", function);
    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(ctx.context, "while.body", function);
    llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(ctx.context, "while.after", function);

    // 创建跳转到 condBlock，连上 while 结构
    ctx.builder.CreateBr(condBlock);

    // condBlock
    ctx.builder.SetInsertPoint(condBlock);     // 设置插入点
    ctx.currentBlock = condBlock;

    llvm::Value* condValue = condition->codeGen(ctx);
    if (!condValue) return nullptr;

    if (!condValue->getType()->isIntegerTy(1)) {
        condValue = ctx.builder.CreateICmpNE(condValue, llvm::ConstantInt::get(condValue->getType(), 0), "whilecond");
    }

    ctx.builder.CreateCondBr(condValue, bodyBlock, afterBlock);

    // bodyBlock
    ctx.builder.SetInsertPoint(bodyBlock);
    ctx.currentBlock = bodyBlock;

    loop_body->codeGen(ctx);

    if (!ctx.currentBlock->getTerminator()) {
        ctx.builder.SetInsertPoint(ctx.currentBlock);
        ctx.builder.CreateBr(condBlock);
    }

    // 必须插入 afterBlock，不然后续的代码可能跳不到这里
    ctx.builder.SetInsertPoint(afterBlock);
    ctx.currentBlock = afterBlock;

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

llvm::Value* FuncCallStmt::codeGen(CodeGenContext& ctx) const  
{
    const std::string& funcName = name->ident;

    SymbolInfo* funcSymbol = scope->lookup(funcName);
    if (!funcSymbol || funcSymbol->kind != SymbolKind::Function) {
        reportError("函数: " + funcName + " 未定义");
        return nullptr;
    }
    std::string funcLLVMName = funcSymbol->llvmName;
    llvm::Function* calleeFunc = ctx.module.getFunction(funcLLVMName);

    std::vector<llvm::Value*> argsV;
    if (args) {
        int idx = 0;
        for (const auto& argExpr: args->args) {
            llvm::Value* argVal = argExpr->codeGen(ctx);
            if (!argVal) return nullptr;

            if (idx < static_cast<int>(funcSymbol->paramTypes.size())) {
                const TypeInfo& expected = funcSymbol->paramTypes[idx];
                llvm::Type* expectedType = expected.kind == SymbolKind::Array
                    ? typeInfoToLLVMType(expected, ctx.context, true)
                    : typeInfoToLLVMValueType(expected, ctx.context);
                argVal = castValueToType(argVal, expectedType, ctx);
            }
            argsV.push_back(argVal);
            idx++;
        }
    }

    if (funcSymbol->funcDef) {
        for (auto* captured : funcSymbol->funcDef->captures) {
            if (!captured) continue;
            SymbolInfo* callerSymbol = scope->lookup(captured->name);
            if (!callerSymbol || !callerSymbol->addr) {
                reportError("捕获变量: " + captured->name + " 在调用点不可用");
                return nullptr;
            }
            argsV.push_back(callerSymbol->addr);
        }
    }

    ctx.builder.CreateCall(calleeFunc, argsV);
    return nullptr; // 作为语句，不返回值
}

// 输入语句节点
InputStmt::InputStmt(std::vector<std::unique_ptr<Expr>> idents)
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

llvm::Value* InputStmt::codeGen(CodeGenContext& ctx) const  
{
    llvm::Function* scanfFunc = ctx.module.getFunction("scanf");
    if (!scanfFunc) {
        llvm::FunctionType* scanfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true
        );
        scanfFunc = llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage, "scanf", ctx.module);
    }

    for (auto& ident: idents) {
        assert(scope && "InputStmt::codeGen 中的 scope 为空");
        llvm::Value* addr = nullptr;
        bool expectFloat = false;
        if (auto* idExpr = dynamic_cast<IdentExpr*>(ident.get())) {
            SymbolInfo* symbol = scope->lookup(idExpr->ident);
            if (!symbol) {
                reportError("变量: " + idExpr->ident + " 未声明");
                return nullptr;
            }

            expectFloat = symbol->isFloat;

            if (symbol->kind == SymbolKind::Array) {
                reportError("不支持直接输入数组: " + idExpr->ident);
                continue;
            }

            if (!symbol->addr) {
                reportError("变量: " + idExpr->ident + " 未分配空间");
                return nullptr;
            }
            addr = symbol->addr;
        } else if (auto* arraySubscriptExpr = dynamic_cast<ArraySubscriptExpr*>(ident.get())) {
            addr = arraySubscriptExpr->getAddress(ctx);
            SymbolInfo* arraySymbol = scope->lookup(arraySubscriptExpr->array->ident);
            if (arraySymbol) {
                expectFloat = arraySymbol->isFloat;
            }
            if (!addr) {
                reportError("数组下标访问异常");
            }
        }
        std::string fmt = expectFloat ? "%f" : "%d";
        llvm::Value* formatStr = ctx.builder.CreateGlobalString(fmt);
        ctx.builder.CreateCall(scanfFunc, { formatStr, addr });
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

llvm::Value* OutputStmt::codeGen(CodeGenContext& ctx) const {
    // 获取或声明 printf 函数
    llvm::Function* printfFunc = ctx.module.getFunction("printf");
    if (!printfFunc) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(ctx.context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0),
            true // 可变参数
        );
        printfFunc = llvm::Function::Create(
            printfType,
            llvm::Function::ExternalLinkage,
            "printf",
            ctx.module
        );
    }

    // 构建格式字符串和参数列表
    std::string formatStr;
    std::vector<llvm::Value*> printfArgs;

    for (const auto& expr : idents) {
        llvm::Value* val = expr->codeGen(ctx);
        if (val) {
            if (!formatStr.empty()) {
                formatStr += " "; // 多个数之间空格分隔
            }
            if (val->getType()->isIntegerTy(1)) {
                val = ctx.builder.CreateZExt(val, llvm::Type::getInt32Ty(ctx.context));
            }

            if (val->getType()->isFloatingPointTy()) {
                formatStr += "%f";
                llvm::Value* promoted = ctx.builder.CreateFPExt(val, llvm::Type::getDoubleTy(ctx.context), "fpext_print");
                printfArgs.push_back(promoted);
            } else {
                formatStr += "%d";
                if (val->getType()->isIntegerTy(32)) {
                    printfArgs.push_back(val);
                } else {
                    llvm::Value* casted = castValueToType(val, llvm::Type::getInt32Ty(ctx.context), ctx);
                    printfArgs.push_back(casted);
                }
            }
        }
    }

    if (!formatStr.empty()) {
        formatStr += "\n"; // 行末换行
        llvm::Value* formatStrVal = ctx.builder.CreateGlobalString(formatStr);
        printfArgs.insert(printfArgs.begin(), formatStrVal); // 格式串是第一个参数
        ctx.builder.CreateCall(printfFunc, printfArgs);
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

llvm::Value* BoolExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* lhsVal = lhs->codeGen(ctx);
    llvm::Value* rhsVal = rhs->codeGen(ctx);

    if (!lhsVal || !rhsVal) {
        reportError("布尔表达式左右子表达式生成失败");
        return nullptr;
    }

    bool useFloat = lhsVal->getType()->isFloatingPointTy() || rhsVal->getType()->isFloatingPointTy();
    if (useFloat) {
        lhsVal = castValueToType(lhsVal, llvm::Type::getFloatTy(ctx.context), ctx);
        rhsVal = castValueToType(rhsVal, llvm::Type::getFloatTy(ctx.context), ctx);
        if (symbol == "==") {
            return ctx.builder.CreateFCmpOEQ(lhsVal, rhsVal, "fcmp_eq");
        } else if (symbol == "!=") {
            return ctx.builder.CreateFCmpONE(lhsVal, rhsVal, "fcmp_ne");
        } else if (symbol == "<") {
            return ctx.builder.CreateFCmpOLT(lhsVal, rhsVal, "fcmp_lt");
        } else if (symbol == "<=") {
            return ctx.builder.CreateFCmpOLE(lhsVal, rhsVal, "fcmp_le");
        } else if (symbol == ">") {
            return ctx.builder.CreateFCmpOGT(lhsVal, rhsVal, "fcmp_gt");
        } else if (symbol == ">=") {
            return ctx.builder.CreateFCmpOGE(lhsVal, rhsVal, "fcmp_ge");
        }
        reportError("不支持的布尔操作符");
        return nullptr;
    }

    lhsVal = castValueToType(lhsVal, llvm::Type::getInt32Ty(ctx.context), ctx);
    rhsVal = castValueToType(rhsVal, llvm::Type::getInt32Ty(ctx.context), ctx);
    if (symbol == "==") {
        return ctx.builder.CreateICmpEQ(lhsVal, rhsVal, "cmpeq");
    } else if (symbol == "!=") {
        return ctx.builder.CreateICmpNE(lhsVal, rhsVal, "cmpne");
    } else if (symbol == "<") {
        return ctx.builder.CreateICmpSLT(lhsVal, rhsVal, "cmplt");
    } else if (symbol == "<=") {
        return ctx.builder.CreateICmpSLE(lhsVal, rhsVal, "cmple");
    } else if (symbol == ">") {
        return ctx.builder.CreateICmpSGT(lhsVal, rhsVal, "cmpgt");
    } else if (symbol == ">=") {
        return ctx.builder.CreateICmpSGE(lhsVal, rhsVal, "cmpge");
    }

    reportError("不支持的布尔操作符");
    return nullptr;
}

// 整数常量节点
NumberExpr::NumberExpr(int val) : value(val) {}

void NumberExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Number(" << value << ")" << std::endl;
}

llvm::Value* NumberExpr::codeGen(CodeGenContext& ctx) const
{
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), value);
}

// 浮点常量节点
FloatNumberExpr::FloatNumberExpr(double val) : value(val) {}

void FloatNumberExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Float(" << value << ")" << std::endl;
}

llvm::Value* FloatNumberExpr::codeGen(CodeGenContext& ctx) const
{
    return llvm::ConstantFP::get(llvm::Type::getFloatTy(ctx.context), value);
}

// 一元运算符节点
UnaryExpr::UnaryExpr(char op, std::unique_ptr<Expr> rhs): op(op), rhs(std::move(rhs)) {}

void UnaryExpr::print(int indent) const  
{
    std::cout << std::string(indent, ' ') << "Unary(" << op << ")" << std::endl;
    rhs->print(indent+2);
}

llvm::Value* UnaryExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* RHS = rhs->codeGen(ctx);
    switch (op) {
    case '+':
        return RHS;
    case '-':
        if (RHS->getType()->isFloatingPointTy()) {
            return ctx.builder.CreateFNeg(RHS);
        }
        return ctx.builder.CreateNeg(RHS);
    default:
        return nullptr;
    }
}

AddressOfExpr::AddressOfExpr(std::unique_ptr<Expr> target)
    : target(std::move(target)) {}

void AddressOfExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "AddressOf" << std::endl;
    target->print(indent + 2);
}

llvm::Value* AddressOfExpr::codeGen(CodeGenContext& ctx) const
{
    if (auto ident = dynamic_cast<IdentExpr*>(target.get())) {
        SymbolInfo* symbol = scope->lookup(ident->ident);
        if (!symbol || !symbol->addr) {
            reportError("变量未声明或未分配空间: " + ident->ident);
            return nullptr;
        }
        if (symbol->kind == SymbolKind::Array) {
            // 取数组首元素地址
            std::vector<llvm::Value*> indices;
            indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
            for (size_t i = 0; i < symbol->dimensions.size(); ++i) {
                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
            }
            llvm::Type* arrayType = buildArrayType(llvm::Type::getInt32Ty(ctx.context), symbol->dimensions);
            return ctx.builder.CreateGEP(arrayType, symbol->addr, indices, ident->ident + "_addr");
        }
        return symbol->addr;
    }
    if (auto subscript = dynamic_cast<ArraySubscriptExpr*>(target.get())) {
        return subscript->getAddress(ctx);
    }
    reportError("无法对该表达式取地址");
    return nullptr;
}

DereferenceExpr::DereferenceExpr(std::unique_ptr<Expr> pointerExpr)
    : pointerExpr(std::move(pointerExpr)) {}

void DereferenceExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Deref" << std::endl;
    pointerExpr->print(indent + 2);
}

llvm::Value* DereferenceExpr::getPointerValue(CodeGenContext& ctx) const
{
    llvm::Value* ptrVal = pointerExpr->codeGen(ctx);
    if (!ptrVal || !ptrVal->getType()->isPointerTy()) {
        reportError("尝试解引用非指针类型");
        return nullptr;
    }
    return ptrVal;
}

llvm::Value* DereferenceExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* ptrVal = getPointerValue(ctx);
    if (!ptrVal) return nullptr;
    TypeInfo pointeeInfo = evaluateExprType(this);
    llvm::Type* loadType = typeInfoToLLVMValueType(pointeeInfo, ctx.context);
    return ctx.builder.CreateLoad(loadType, ptrVal, "deref");
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

llvm::Value* BinaryExpr::codeGen(CodeGenContext& ctx) const 
{
    llvm::Value* LHS = lhs->codeGen(ctx);
    llvm::Value* RHS = rhs->codeGen(ctx);
    if (!LHS || !RHS) {
        reportError("二元运算的子表达式生成失败");
        return nullptr;
    }

    bool useFloat = LHS->getType()->isFloatingPointTy() || RHS->getType()->isFloatingPointTy();
    if (useFloat) {
        LHS = castValueToType(LHS, llvm::Type::getFloatTy(ctx.context), ctx);
        RHS = castValueToType(RHS, llvm::Type::getFloatTy(ctx.context), ctx);
        switch (op) {
        case '+':
            return ctx.builder.CreateFAdd(LHS, RHS, "faddtmp");
        case '-':
            return ctx.builder.CreateFSub(LHS, RHS, "fsubtmp");
        case '*':
            return ctx.builder.CreateFMul(LHS, RHS, "fmultmp");
        case '/':
            return ctx.builder.CreateFDiv(LHS, RHS, "fdivtmp");
        case '%':
            reportError("浮点数不支持取模运算");
            return nullptr;
        default:
            reportError("不支持的二元运算符: " + std::string(1, op));
            return nullptr;
        }
    }

    LHS = castValueToType(LHS, llvm::Type::getInt32Ty(ctx.context), ctx);
    RHS = castValueToType(RHS, llvm::Type::getInt32Ty(ctx.context), ctx);
    switch (op) {
    case '+':
        return ctx.builder.CreateAdd(LHS, RHS, "addtmp");
    case '-':
        return ctx.builder.CreateSub(LHS, RHS, "subtmp");
    case '*':
        return ctx.builder.CreateMul(LHS, RHS, "multmp");
    case '/':
        return ctx.builder.CreateSDiv(LHS, RHS, "divtmp");
    case '%':
        return ctx.builder.CreateSRem(LHS, RHS, "modtmp");
    default:
        reportError("不支持的二元运算符: " + std::string(1, op));
        return nullptr;
    }
}

// 数组下标访问运算节点
ArraySubscriptExpr::ArraySubscriptExpr(std::unique_ptr<IdentExpr> array, std::vector<std::unique_ptr<Expr>> subscript)
    : array(std::move(array)), subscript(std::move(subscript)) {}

void ArraySubscriptExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Array Subscript(" << array->ident << "[" << std::endl;
    for (int i = 0; i < subscript.size(); i++) {
        subscript[i]->print(indent + 2);
    }
    std::cout << std::string(indent, ' ') << "])" << std::endl;
}

llvm::Value* ArraySubscriptExpr::codeGen(CodeGenContext& ctx) const {
    SymbolInfo* symbol = scope->lookup(array->ident);
    if (!symbol) {
        reportError("数组: " + array->ident + " 未声明");
        return nullptr;
    }

    llvm::Value* arrayAlloca = symbol->addr;
    llvm::Value* arrayPtr = nullptr;

    // 构造数组的完整类型：[d1 x [d2 x ... [dn x i32]]]
    llvm::Type* elementType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
    for (int i = symbol->dimensions.size() - 1; i >= 0; --i) {
        elementType = llvm::ArrayType::get(elementType, symbol->dimensions[i]);
    }

    if (symbol->isFuncParam) {
        // 函数参数情况，形如：alloca ptr -> store ptr to array
        arrayPtr = ctx.builder.CreateLoad(llvm::PointerType::get(elementType, 0), arrayAlloca, array->ident + "_loaded");
    } else {
        // 本地变量（alloca 的就是数组）
        arrayPtr = arrayAlloca;
    }

    if (!arrayPtr) {
        reportError("数组: " + array->ident + " 未分配空间");
        return nullptr;
    }

    // 计算下标
    std::vector<llvm::Value*> indices;
    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
    for (auto& expr: subscript) {
        llvm::Value* index = expr->codeGen(ctx);
        if (index) {
            indices.push_back(index);
        }
    }

    if (indices.size() - 1 != symbol->dimensions.size()) {
        reportError("数组维度不匹配，无法访问: " + std::to_string(indices.size() - 1) + " != " + std::to_string(symbol->dimensions.size()));
        return nullptr;
    }

    llvm::Value* gep = ctx.builder.CreateGEP(
        elementType,
        arrayPtr,
        indices,
        "array_elem"
    );

    llvm::Type* valueType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
    return ctx.builder.CreateLoad(valueType, gep, "load_elem");
}

llvm::Value* ArraySubscriptExpr::getAddress(CodeGenContext& ctx) const {
    SymbolInfo* symbol = scope->lookup(array->ident);
    if (!symbol) {
        reportError("数组: " + array->ident + " 未声明");
        return nullptr;
    }

    llvm::Value* arrayAlloca = symbol->addr;
    llvm::Value* arrayPtr = nullptr;

    // 构造完整数组类型
    llvm::Type* elementType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
    for (int i = symbol->dimensions.size() - 1; i >= 0; --i) {
        elementType = llvm::ArrayType::get(elementType, symbol->dimensions[i]);
    }

    if (symbol->isFuncParam) {
        arrayPtr = ctx.builder.CreateLoad(llvm::PointerType::get(elementType, 0), arrayAlloca, array->ident + "_loaded");
    } else {
        arrayPtr = arrayAlloca;
    }

    if (!arrayPtr) {
        reportError("数组: " + array->ident + " 未分配空间");
        return nullptr;
    }

    // 构造 GEP 索引
    std::vector<llvm::Value*> indices;
    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0));
    for (const auto& expr : subscript) {
        llvm::Value* idxVal = expr->codeGen(ctx);
        if (!idxVal) {
            reportError("存在无法作为下标的符号");
            return nullptr;  // 错误处理
        }
        indices.push_back(idxVal);
    }

    llvm::Value* gep = ctx.builder.CreateGEP(
        elementType,
        arrayPtr,
        indices,
        "array_elem"
    );

    return gep;
}

// 成员访问表达式
MemberAccessExpr::MemberAccessExpr(std::unique_ptr<Expr> target, std::unique_ptr<IdentExpr> member)
    : target(std::move(target))
    , member(std::move(member)) {}

void MemberAccessExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "MemberAccess" << std::endl;
    target->print(indent + 2);
    member->print(indent + 2);
}

llvm::Value* MemberAccessExpr::getPointer(CodeGenContext& ctx) const
{
    llvm::Value* baseValue = target->codeGen(ctx);
    TypeInfo baseType = evaluateExprType(target.get());
    if (baseType.pointerLevel > 0) {
        baseType.pointerLevel -= 1;
    }
    auto layoutIt = classFieldLayouts.find(baseType.className);
    if (layoutIt == classFieldLayouts.end()) {
        reportError("无法找到类布局：" + baseType.className);
        return nullptr;
    }
    int index = -1;
    for (size_t i = 0; i < layoutIt->second.size(); ++i) {
        if (layoutIt->second[i].first == member->ident) {
            index = static_cast<int>(i);
            break;
        }
    }
    if (index < 0) {
        reportError("成员不存在：" + member->ident);
        return nullptr;
    }

    llvm::Value* ptr = baseValue;
    if (!ptr->getType()->isPointerTy()) {
        auto* tmp = ctx.builder.CreateAlloca(ptr->getType());
        ctx.builder.CreateStore(ptr, tmp);
        ptr = tmp;
    }

    llvm::StructType* structTy = classStructTypes[baseType.className];
    if (!structTy) {
        reportError("无法找到类类型：" + baseType.className);
        return nullptr;
    }

    llvm::PointerType* targetPtrTy = llvm::PointerType::get(structTy, 0);
    if (ptr->getType() != targetPtrTy) {
        ptr = ctx.builder.CreateBitCast(ptr, targetPtrTy);
    }
    return ctx.builder.CreateStructGEP(structTy, ptr, index, "fieldptr");
}

llvm::Value* MemberAccessExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* ptr = getPointer(ctx);
    if (!ptr) return nullptr;
    TypeInfo baseType = evaluateExprType(target.get());
    if (baseType.pointerLevel > 0) {
        baseType.pointerLevel -= 1;
    }
    auto layoutIt = classFieldLayouts.find(baseType.className);
    if (layoutIt == classFieldLayouts.end()) {
        reportError("无法找到类布局：" + baseType.className);
        return nullptr;
    }
    int index = -1;
    for (size_t i = 0; i < layoutIt->second.size(); ++i) {
        if (layoutIt->second[i].first == member->ident) {
            index = static_cast<int>(i);
            break;
        }
    }
    if (index < 0) {
        reportError("成员不存在：" + member->ident);
        return nullptr;
    }

    llvm::Type* fieldTy = typeInfoToLLVMType(layoutIt->second[index].second, ctx.context, true);
    if (!fieldTy) {
        reportError("无法得到字段类型：" + member->ident);
        return nullptr;
    }

    return ctx.builder.CreateLoad(fieldTy, ptr, "fieldload");
}

// 方法调用
MethodCallExpr::MethodCallExpr(std::unique_ptr<Expr> target, std::unique_ptr<IdentExpr> method, std::unique_ptr<ArgList> args)
    : target(std::move(target))
    , method(std::move(method))
    , args(std::move(args)) {}

void MethodCallExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "MethodCall" << std::endl;
    target->print(indent + 2);
    method->print(indent + 2);
    if (args) args->print(indent + 2);
}

llvm::Value* MethodCallExpr::codeGen(CodeGenContext& ctx) const
{
    llvm::Value* baseValue = target->codeGen(ctx);
    TypeInfo baseType = evaluateExprType(target.get());
    llvm::StructType* structTy = classStructTypes[baseType.className];
    if (!structTy) {
        reportError("无法找到方法所属的类类型");
        return nullptr;
    }
    llvm::Value* thisPtr = baseValue;
    if (!thisPtr->getType()->isPointerTy()) {
        auto* tmp = ctx.builder.CreateAlloca(thisPtr->getType());
        ctx.builder.CreateStore(thisPtr, tmp);
        thisPtr = tmp;
    }
    std::vector<llvm::Value*> callArgs;
    callArgs.push_back(thisPtr);
    if (args) {
        for (const auto& arg : args->args) {
            callArgs.push_back(arg->codeGen(ctx));
        }
    }
    std::string funcName = baseType.className + "." + method->ident;
    llvm::Function* callee = ctx.module.getFunction(funcName);
    if (!callee) {
        reportError("未找到方法定义：" + funcName);
        return nullptr;
    }
    return ctx.builder.CreateCall(callee, callArgs, "methodcall");
}

// new 表达式
NewExpr::NewExpr(std::unique_ptr<IdentExpr> className, std::unique_ptr<ArgList> args)
    : className(std::move(className))
    , args(std::move(args)) {}

void NewExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "NewExpr(" << className->ident << ")" << std::endl;
    if (args) args->print(indent + 2);
}

llvm::Value* NewExpr::codeGen(CodeGenContext& ctx) const
{
    reportError("暂未实现 new 表达式的代码生成");
    return nullptr;
}

// 标识符节点
IdentExpr::IdentExpr(const std::string& ident, TypeInfo type) : ident(ident), type(type) {}

void IdentExpr::print(int indent) const 
{
    switch (type.kind) {
    case SymbolKind::Int:
    case SymbolKind::Float:
    case SymbolKind::Function:
    case SymbolKind::Program:
        std::cout << std::string(indent, ' ')
            << "Ident(" << ident << ": "
            << SymbolName[static_cast<int>(type.kind)] << ")" << std::endl;
        break;
    case SymbolKind::Pointer:
        std::cout << std::string(indent, ' ')
            << "Ident(" << ident << ": Pointer^" << std::max(1, type.pointerLevel) << ")" << std::endl;
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

llvm::Value* IdentExpr::codeGen(CodeGenContext& ctx) const 
{
    SymbolInfo* symbol = scope->lookup(ident);

    if (!symbol) {
        reportError("标识符: " + ident + " 不存在");
        return nullptr;
    }

    // 如果为变量
    if (symbol->kind == SymbolKind::Int || symbol->kind == SymbolKind::Float) {
        llvm::Type* valueType = symbol->isFloat ? llvm::Type::getFloatTy(ctx.context) : llvm::Type::getInt32Ty(ctx.context);
        return ctx.builder.CreateLoad(valueType, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::Pointer) {
        TypeInfo symbolType{ SymbolKind::Pointer, {}, symbol->pointerLevel, symbol->isFloat };
        llvm::Type* valueType = typeInfoToLLVMValueType(symbolType, ctx.context);
        return ctx.builder.CreateLoad(valueType, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::Class) {
        TypeInfo symbolType{ SymbolKind::Class, symbol->dimensions, symbol->pointerLevel, symbol->isFloat, symbol->className };
        llvm::Type* valueType = typeInfoToLLVMValueType(symbolType, ctx.context);
        return ctx.builder.CreateLoad(valueType, symbol->addr, ident);
    } else if (symbol->kind == SymbolKind::Array) {
        return symbol->addr;
    }

    reportError("不支持返回的标识符: " + ident);
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

llvm::Value* FuncCallExpr::codeGen(CodeGenContext& ctx) const  
{
    const std::string& funcName = name->ident;

    SymbolInfo* funcSymbol = scope->lookup(funcName);
    if (!funcSymbol || funcSymbol->kind != SymbolKind::Function) {
        reportError("函数: " + funcName + " 未定义");
        return nullptr;
    }
    std::string funcLLVMName = funcSymbol->llvmName;
    llvm::Function* calleeFunc = ctx.module.getFunction(funcLLVMName);

    std::vector<llvm::Value*> argsV;
    if (args) {
        int idx = 0;
        for (const auto& argExpr: args->args) {
            llvm::Value* argVal = argExpr->codeGen(ctx);
            if (!argVal) return nullptr;

            if (idx < static_cast<int>(funcSymbol->paramTypes.size())) {
                const TypeInfo& expected = funcSymbol->paramTypes[idx];
                llvm::Type* expectedType = expected.kind == SymbolKind::Array
                    ? typeInfoToLLVMType(expected, ctx.context, true)
                    : typeInfoToLLVMValueType(expected, ctx.context);
                argVal = castValueToType(argVal, expectedType, ctx);
            }
            argsV.push_back(argVal);
            idx++;
        }
    }

    if (funcSymbol->funcDef) {
        for (auto* captured : funcSymbol->funcDef->captures) {
            if (!captured) continue;
            SymbolInfo* callerSymbol = scope->lookup(captured->name);
            if (!callerSymbol || !callerSymbol->addr) {
                reportError("捕获变量: " + captured->name + " 在调用点不可用");
                return nullptr;
            }
            argsV.push_back(callerSymbol->addr);
        }
    }

    if (funcSymbol->funcDef) {
        for (auto* captured : funcSymbol->funcDef->captures) {
            if (!captured) continue;
            SymbolInfo* callerSymbol = scope->lookup(captured->name);
            if (!callerSymbol || !callerSymbol->addr) {
                reportError("捕获变量: " + captured->name + " 在调用点不可用");
                return nullptr;
            }
            argsV.push_back(callerSymbol->addr);
        }
    }

    return ctx.builder.CreateCall(calleeFunc, argsV, funcName + "_call");
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

llvm::Value* ArgList::codeGen(CodeGenContext& ctx) const 
{
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

llvm::Value* ParamList::codeGen(CodeGenContext& ctx) const 
{
    return nullptr;
}

// 输入函数参数列表节点
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