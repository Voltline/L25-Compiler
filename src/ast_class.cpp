#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

// 当前正在代码生成的类名（仅本文件使用）
static std::string currentClassNameCodegen;

// ===== 类字段 =====
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

// ===== 构造函数 =====
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
    if (currentClassNameCodegen.empty()) {
        reportError("构造函数未关联类");
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

    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), argTypes, false);
    size_t paramCount = params ? params->params.size() : 0;
    std::string funcName = buildCtorName(currentClassNameCodegen, paramCount);
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

    ctx.pushCleanupScope();
    if (body) {
        for (const auto& stmt : body->stmts) {
            stmt->codeGen(ctx);
        }
    }

    emitScopeCleanup(ctx);
    ctx.builder.CreateRetVoid();
    return function;
}

// ===== 析构函数 =====
DtorDecl::DtorDecl(std::unique_ptr<IdentExpr> name, std::unique_ptr<StmtList> body)
    : name(std::move(name))
    , body(std::move(body))
    , bodyScope(nullptr) {}

void DtorDecl::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "DtorDecl" << std::endl;
    name->print(indent + 2);
    if (body) body->print(indent + 2);
}

llvm::Value* DtorDecl::codeGen(CodeGenContext& ctx) const
{
    if (currentClassNameCodegen.empty()) {
        reportError("析构函数未关联类");
        return nullptr;
    }

    llvm::StructType* classTy = classStructTypes[currentClassNameCodegen];
    if (!classTy) {
        reportError("找不到类类型：" + currentClassNameCodegen);
        return nullptr;
    }

    std::vector<llvm::Type*> argTypes;
    argTypes.push_back(llvm::PointerType::get(classTy, 0));

    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.context), argTypes, false);
    std::string funcName = buildDtorName(currentClassNameCodegen);
    llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcName, ctx.module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx.context, "entry", function);
    ctx.builder.SetInsertPoint(entry);

    auto argIt = function->arg_begin();
    if (argIt != function->arg_end()) {
        argIt->setName("this");
        SymbolInfo* thisInfo = bodyScope ? bodyScope->lookupLocal("this") : nullptr;
        if (thisInfo) {
            llvm::AllocaInst* alloca = ctx.builder.CreateAlloca(argIt->getType(), nullptr, "this.addr");
            ctx.builder.CreateStore(&*argIt, alloca);
            thisInfo->addr = alloca;
            thisInfo->value = &*argIt;
        }
    }

    ctx.pushCleanupScope();
    if (body) {
        for (const auto& stmt : body->stmts) {
            stmt->codeGen(ctx);
        }
    }

    emitScopeCleanup(ctx);
    ctx.builder.CreateRetVoid();
    return function;
}

// ===== 方法 =====
MethodDecl::MethodDecl(std::unique_ptr<IdentExpr> name, std::unique_ptr<ParamList> params, std::unique_ptr<StmtList> body, std::unique_ptr<Expr> return_value, TypeInfo returnType)
    : name(std::move(name))
    , params(std::move(params))
    , body(std::move(body))
    , return_value(std::move(return_value))
    , returnType(std::move(returnType))
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

    llvm::Type* retType = typeInfoToLLVMValueType(returnType, ctx.context);
    if (!retType) retType = llvm::Type::getInt32Ty(ctx.context);
    llvm::FunctionType* funcType = llvm::FunctionType::get(retType, argTypes, false);
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

    ctx.pushCleanupScope();
    if (body) {
        for (const auto& stmt : body->stmts) {
            stmt->codeGen(ctx);
        }
    }

    llvm::Value* retVal = return_value ? return_value->codeGen(ctx) : defaultValueForType(returnType, ctx);

    // 如果返回字符串变量，先将其数据置空以阻止清理释放返回值
    if (return_value) {
        if (returnType.kind == SymbolKind::String && returnType.pointerLevel == 0) {
            if (auto* identRet = dynamic_cast<IdentExpr*>(return_value.get())) {
                SymbolInfo* sym = bodyScope->lookup(identRet->ident);
                if (sym && sym->addr) {
                    llvm::StructType* strTy = getL25StringType(ctx.context);
                    ctx.builder.CreateStore(llvm::ConstantAggregateZero::get(strTy), sym->addr);
                }
            } else if (!isOwnedStringExpr(return_value.get())) {
                retVal = emitStringDeepCopy(retVal, ctx);
            }
        } else if (returnType.kind == SymbolKind::Class && returnType.pointerLevel > 0) {
            if (auto* identRet = dynamic_cast<IdentExpr*>(return_value.get())) {
                SymbolInfo* sym = bodyScope->lookup(identRet->ident);
                if (sym && sym->addr) {
                    llvm::Type* ptrTy = retVal->getType();
                    ctx.builder.CreateStore(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(ptrTy)), sym->addr);
                }
            }
        }
    }

    emitScopeCleanup(ctx);
    retVal = castValueToType(retVal, retType, ctx);
    ctx.builder.CreateRet(retVal);
    return function;
}

// ===== 类声明 =====
ClassDecl::ClassDecl(std::unique_ptr<IdentExpr> name,
                     std::unique_ptr<IdentExpr> baseClass,
                     std::vector<std::unique_ptr<FieldDecl>> fields,
                     std::vector<std::unique_ptr<MethodDecl>> methods,
                     std::vector<std::unique_ptr<CtorDecl>> ctors,
                     std::unique_ptr<DtorDecl> dtor)
    : name(std::move(name))
    , baseClass(std::move(baseClass))
    , fields(std::move(fields))
    , methods(std::move(methods))
    , ctors(std::move(ctors))
    , dtor(std::move(dtor)) {}

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
    if (dtor) {
        dtor->print(indent + 2);
    }
    for (const auto& method : methods) {
        method->print(indent + 2);
    }
}

llvm::Value* ClassDecl::codeGen(CodeGenContext& ctx) const
{
    llvm::StructType* structTy = nullptr;
    auto structIt = classStructTypes.find(name->ident);
    if (structIt != classStructTypes.end()) {
        structTy = structIt->second;
    } else {
        structTy = llvm::StructType::create(ctx.context, name->ident);
        classStructTypes[name->ident] = structTy;
    }

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
    structTy->setBody(fieldTypes, false);
    classFieldLayouts[name->ident] = layout;

    std::string saved = currentClassNameCodegen;
    currentClassNameCodegen = name->ident;
    for (const auto& ctor : ctors) {
        ctor->codeGen(ctx);
    }
    if (dtor) {
        dtor->codeGen(ctx);
    }
    for (const auto& method : methods) {
        method->codeGen(ctx);
    }
    currentClassNameCodegen = saved;

    // ===== 生成反射查找表 =====
    llvm::StructType* strTy = getL25StringType(ctx.context);
    auto i32Ty = llvm::Type::getInt32Ty(ctx.context);

    // 字段名数组
    {
        auto layoutIt = classFieldLayouts.find(name->ident);
        if (layoutIt != classFieldLayouts.end() && !layoutIt->second.empty()) {
            std::vector<llvm::Constant*> elements;
            for (const auto& [fname, ftype] : layoutIt->second) {
                auto* strData = llvm::ConstantDataArray::getString(ctx.context, fname, true);
                auto* gv = new llvm::GlobalVariable(
                    ctx.module, strData->getType(), true,
                    llvm::GlobalValue::PrivateLinkage, strData,
                    ".reflect_fn_" + name->ident + "_" + fname);
                auto* lenC = llvm::ConstantInt::get(i32Ty, static_cast<int>(fname.size()));
                auto* ptrC = llvm::ConstantExpr::getInBoundsGetElementPtr(
                    strData->getType(), gv,
                    llvm::ArrayRef<llvm::Constant*>{
                        llvm::ConstantInt::get(i32Ty, 0),
                        llvm::ConstantInt::get(i32Ty, 0)});
                elements.push_back(llvm::ConstantStruct::get(strTy, { lenC, ptrC }));
            }
            auto* arrTy = llvm::ArrayType::get(strTy, elements.size());
            auto* arrConst = llvm::ConstantArray::get(arrTy, elements);
            new llvm::GlobalVariable(
                ctx.module, arrTy, true,
                llvm::GlobalValue::PrivateLinkage, arrConst,
                "__l25_reflect_fields_" + name->ident);
        }
    }

    // 方法名数组
    {
        auto mnIt = classMethodNames.find(name->ident);
        if (mnIt != classMethodNames.end() && !mnIt->second.empty()) {
            std::vector<llvm::Constant*> elements;
            for (const auto& mname : mnIt->second) {
                auto* strData = llvm::ConstantDataArray::getString(ctx.context, mname, true);
                auto* gv = new llvm::GlobalVariable(
                    ctx.module, strData->getType(), true,
                    llvm::GlobalValue::PrivateLinkage, strData,
                    ".reflect_mn_" + name->ident + "_" + mname);
                auto* lenC = llvm::ConstantInt::get(i32Ty, static_cast<int>(mname.size()));
                auto* ptrC = llvm::ConstantExpr::getInBoundsGetElementPtr(
                    strData->getType(), gv,
                    llvm::ArrayRef<llvm::Constant*>{
                        llvm::ConstantInt::get(i32Ty, 0),
                        llvm::ConstantInt::get(i32Ty, 0)});
                elements.push_back(llvm::ConstantStruct::get(strTy, { lenC, ptrC }));
            }
            auto* arrTy = llvm::ArrayType::get(strTy, elements.size());
            auto* arrConst = llvm::ConstantArray::get(arrTy, elements);
            new llvm::GlobalVariable(
                ctx.module, arrTy, true,
                llvm::GlobalValue::PrivateLinkage, arrConst,
                "__l25_reflect_methods_" + name->ident);
        }
    }

    return nullptr;
}
