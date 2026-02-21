#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

// ===== 反射：辅助函数 - 从表达式推导类名 =====
static std::string resolveClassNameFromExpr(const Expr* expr)
{
    TypeInfo ti = evaluateExprType(expr);
    // 允许类实例（pointerLevel==0）或类指针（pointerLevel>0）
    if (ti.kind == SymbolKind::Class && !ti.className.empty()) {
        return ti.className;
    }
    return "";
}

// ===== 反射：辅助函数 - 生成 __l25_string 常量值 =====
static llvm::Value* buildStringConstant(CodeGenContext& ctx, const std::string& str)
{
    llvm::StructType* strTy = getL25StringType(ctx.context);
    llvm::Constant* strConst = ctx.builder.CreateGlobalString(str, ".reflect_str");
    llvm::Value* result = llvm::UndefValue::get(strTy);
    llvm::Value* lenVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context),
                                                  static_cast<int>(str.size()));
    result = ctx.builder.CreateInsertValue(result, lenVal, 0, "str_set_len");
    result = ctx.builder.CreateInsertValue(result, strConst, 1, "str_set_data");
    return result;
}

// ===== 反射：typename(expr) =====
TypenameExpr::TypenameExpr(std::unique_ptr<Expr> target) : target(std::move(target)) {}

void TypenameExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Typename" << std::endl;
    if (target) target->print(indent + 2);
}

llvm::Value* TypenameExpr::codeGen(CodeGenContext& ctx) const
{
    std::string className = resolveClassNameFromExpr(target.get());
    if (className.empty()) {
        // 非类类型，返回基础类型名称
        TypeInfo ti = evaluateExprType(target.get());
        if (ti.kind == SymbolKind::Int) className = "int";
        else if (ti.kind == SymbolKind::Float) className = "float";
        else if (ti.kind == SymbolKind::String) className = "string";
        else className = "unknown";
    }
    // 生成目标表达式（可能有副作用）
    target->codeGen(ctx);
    return buildStringConstant(ctx, className);
}

// ===== 反射：fieldcount(expr) =====
FieldCountExpr::FieldCountExpr(std::unique_ptr<Expr> target) : target(std::move(target)) {}

void FieldCountExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "FieldCount" << std::endl;
    if (target) target->print(indent + 2);
}

llvm::Value* FieldCountExpr::codeGen(CodeGenContext& ctx) const
{
    std::string className = resolveClassNameFromExpr(target.get());
    if (className.empty()) {
        reportError("fieldcount 仅支持类类型参数");
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }
    auto it = classFieldLayouts.find(className);
    int count = (it != classFieldLayouts.end()) ? static_cast<int>(it->second.size()) : 0;
    target->codeGen(ctx);
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), count);
}

// ===== 反射：methodcount(expr) =====
MethodCountExpr::MethodCountExpr(std::unique_ptr<Expr> target) : target(std::move(target)) {}

void MethodCountExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "MethodCount" << std::endl;
    if (target) target->print(indent + 2);
}

llvm::Value* MethodCountExpr::codeGen(CodeGenContext& ctx) const
{
    std::string className = resolveClassNameFromExpr(target.get());
    if (className.empty()) {
        reportError("methodcount 仅支持类类型参数");
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }
    auto it = classMethodNames.find(className);
    int count = (it != classMethodNames.end()) ? static_cast<int>(it->second.size()) : 0;
    target->codeGen(ctx);
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), count);
}

// ===== 反射：fieldname(expr, index) =====
FieldNameExpr::FieldNameExpr(std::unique_ptr<Expr> target, std::unique_ptr<Expr> index)
    : target(std::move(target)), index(std::move(index)) {}

void FieldNameExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "FieldName" << std::endl;
    if (target) target->print(indent + 2);
    if (index) index->print(indent + 2);
}

llvm::Value* FieldNameExpr::codeGen(CodeGenContext& ctx) const
{
    std::string className = resolveClassNameFromExpr(target.get());
    if (className.empty()) {
        reportError("fieldname 仅支持类类型参数");
        return buildStringConstant(ctx, "");
    }
    auto it = classFieldLayouts.find(className);
    if (it == classFieldLayouts.end() || it->second.empty()) {
        reportError("fieldname: 类 " + className + " 无字段");
        return buildStringConstant(ctx, "");
    }
    int fieldCount = static_cast<int>(it->second.size());

    // 编译期常量快速路径
    if (auto numExpr = dynamic_cast<const NumberExpr*>(index.get())) {
        int idx = numExpr->value;
        if (idx < 0 || idx >= fieldCount) {
            reportError("fieldname 索引越界：" + std::to_string(idx));
            return buildStringConstant(ctx, "");
        }
        target->codeGen(ctx);
        return buildStringConstant(ctx, it->second[idx].first);
    }

    // 运行时索引：从全局查找表加载
    target->codeGen(ctx);
    llvm::Value* idxVal = index->codeGen(ctx);
    if (!idxVal) {
        reportError("fieldname 索引表达式生成失败");
        return buildStringConstant(ctx, "");
    }
    if (idxVal->getType()->isIntegerTy(32)) {
        idxVal = ctx.builder.CreateSExt(idxVal, llvm::Type::getInt64Ty(ctx.context), "idx_ext");
    }

    llvm::StructType* strTy = getL25StringType(ctx.context);
    auto* arrTy = llvm::ArrayType::get(strTy, fieldCount);
    auto* gv = ctx.module.getNamedGlobal("__l25_reflect_fields_" + className);
    if (!gv) {
        reportError("fieldname: 反射查找表未生成");
        return buildStringConstant(ctx, "");
    }
    llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), 0);
    llvm::Value* elemPtr = ctx.builder.CreateInBoundsGEP(arrTy, gv, { zero, idxVal }, "field_name_ptr");
    return ctx.builder.CreateLoad(strTy, elemPtr, "field_name_val");
}

// ===== 反射：methodname(expr, index) =====
MethodNameExpr::MethodNameExpr(std::unique_ptr<Expr> target, std::unique_ptr<Expr> index)
    : target(std::move(target)), index(std::move(index)) {}

void MethodNameExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "MethodName" << std::endl;
    if (target) target->print(indent + 2);
    if (index) index->print(indent + 2);
}

llvm::Value* MethodNameExpr::codeGen(CodeGenContext& ctx) const
{
    std::string className = resolveClassNameFromExpr(target.get());
    if (className.empty()) {
        reportError("methodname 仅支持类类型参数");
        return buildStringConstant(ctx, "");
    }
    auto it = classMethodNames.find(className);
    if (it == classMethodNames.end() || it->second.empty()) {
        reportError("methodname: 类 " + className + " 无方法");
        return buildStringConstant(ctx, "");
    }
    int methodCount = static_cast<int>(it->second.size());

    // 编译期常量快速路径
    if (auto numExpr = dynamic_cast<const NumberExpr*>(index.get())) {
        int idx = numExpr->value;
        if (idx < 0 || idx >= methodCount) {
            reportError("methodname 索引越界：" + std::to_string(idx));
            return buildStringConstant(ctx, "");
        }
        target->codeGen(ctx);
        return buildStringConstant(ctx, it->second[idx]);
    }

    // 运行时索引：从全局查找表加载
    target->codeGen(ctx);
    llvm::Value* idxVal = index->codeGen(ctx);
    if (!idxVal) {
        reportError("methodname 索引表达式生成失败");
        return buildStringConstant(ctx, "");
    }
    if (idxVal->getType()->isIntegerTy(32)) {
        idxVal = ctx.builder.CreateSExt(idxVal, llvm::Type::getInt64Ty(ctx.context), "idx_ext");
    }

    llvm::StructType* strTy = getL25StringType(ctx.context);
    auto* arrTy = llvm::ArrayType::get(strTy, methodCount);
    auto* gv = ctx.module.getNamedGlobal("__l25_reflect_methods_" + className);
    if (!gv) {
        reportError("methodname: 反射查找表未生成");
        return buildStringConstant(ctx, "");
    }
    llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), 0);
    llvm::Value* elemPtr = ctx.builder.CreateInBoundsGEP(arrTy, gv, { zero, idxVal }, "method_name_ptr");
    return ctx.builder.CreateLoad(strTy, elemPtr, "method_name_val");
}

// ===== 反射：invoke(obj, name_expr [, args...]) =====
InvokeExpr::InvokeExpr(std::unique_ptr<Expr> target, std::unique_ptr<Expr> methodName, std::unique_ptr<ArgList> args)
    : target(std::move(target)), methodName(std::move(methodName)), args(std::move(args)) {}

void InvokeExpr::print(int indent) const
{
    std::cout << std::string(indent, ' ') << "Invoke" << std::endl;
    if (target) target->print(indent + 2);
    if (methodName) methodName->print(indent + 2);
    if (args) args->print(indent + 2);
}

llvm::Value* InvokeExpr::codeGen(CodeGenContext& ctx) const
{
    // 1. 解析类名
    std::string className = resolveClassNameFromExpr(target.get());
    if (className.empty()) {
        reportError("invoke 仅支持类类型参数");
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }

    // 2. 获取方法名字符串并提取 char*
    llvm::Value* nameVal = methodName->codeGen(ctx);
    if (!nameVal) {
        reportError("invoke 方法名表达式生成失败");
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }
    llvm::Value* namePtr = ctx.builder.CreateExtractValue(nameVal, 1, "invoke_name_ptr");

    // 3. 获取 this 指针（复用 MethodCallExpr 的逻辑）
    TypeInfo baseType = evaluateExprType(target.get());
    llvm::Value* baseValue = nullptr;
    if (auto ident = dynamic_cast<IdentExpr*>(target.get())) {
        if (SymbolInfo* symbol = ident->scope->lookup(ident->ident)) {
            if (symbol->kind == SymbolKind::Class && symbol->pointerLevel == 0) {
                baseValue = symbol->addr;
            }
        }
    } else if (auto memberAccess = dynamic_cast<MemberAccessExpr*>(target.get())) {
        baseValue = memberAccess->getPointer(ctx);
    } else if (auto arrayAccess = dynamic_cast<ArraySubscriptExpr*>(target.get())) {
        baseValue = arrayAccess->getAddress(ctx);
    }
    if (!baseValue) {
        baseValue = target->codeGen(ctx);
    }

    llvm::StructType* classTy = classStructTypes[baseType.className];
    if (!classTy) {
        reportError("invoke: 无法找到类类型");
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }
    llvm::Value* thisPtr = baseValue;
    if (!thisPtr->getType()->isPointerTy()) {
        auto* tmp = ctx.builder.CreateAlloca(thisPtr->getType());
        ctx.builder.CreateStore(thisPtr, tmp);
        thisPtr = tmp;
    }
    llvm::PointerType* targetPtrTy = llvm::PointerType::get(classTy, 0);
    if (thisPtr->getType() != targetPtrTy) {
        thisPtr = ctx.builder.CreateBitCast(thisPtr, targetPtrTy);
    }

    // 4. 预先生成额外参数
    std::vector<llvm::Value*> extraArgs;
    if (args) {
        for (const auto& arg : args->args) {
            extraArgs.push_back(arg->codeGen(ctx));
        }
    }
    size_t extraArgCount = extraArgs.size();

    // 5. 确保 strcmp 存在
    ensureStringRuntimeDeclared(ctx);
    llvm::Function* strcmpFn = ctx.module.getFunction("strcmp");

    // 6. 收集匹配 arity 的方法
    auto mnIt = classMethodNames.find(className);
    if (mnIt == classMethodNames.end() || mnIt->second.empty()) {
        reportError("invoke: 类 " + className + " 无方法");
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }

    struct MethodCandidate {
        std::string name;
        llvm::Function* func;
    };
    std::vector<MethodCandidate> candidates;
    for (const auto& mname : mnIt->second) {
        std::string funcName = className + "." + mname;
        llvm::Function* fn = ctx.module.getFunction(funcName);
        if (!fn) continue;
        size_t paramCount = fn->arg_size() - 1; // 减去 this
        if (paramCount == extraArgCount) {
            candidates.push_back({mname, fn});
        }
    }

    if (candidates.empty()) {
        reportError("invoke: 未找到参数数量为 " + std::to_string(extraArgCount) + " 的方法");
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context), 0);
    }

    // 7. 创建 result 变量
    auto* i32Ty = llvm::Type::getInt32Ty(ctx.context);
    llvm::AllocaInst* resultAlloca = ctx.builder.CreateAlloca(i32Ty, nullptr, "invoke_result");
    ctx.builder.CreateStore(llvm::ConstantInt::get(i32Ty, 0), resultAlloca);

    // 8. 生成 if-else strcmp 分发链
    llvm::Function* currentFunc = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* endBB = llvm::BasicBlock::Create(ctx.context, "invoke_end", currentFunc);

    for (size_t ci = 0; ci < candidates.size(); ++ci) {
        auto& cand = candidates[ci];

        llvm::Constant* nameStr = ctx.builder.CreateGlobalString(cand.name, ".invoke_cmp_" + cand.name);
        llvm::Value* cmpResult = ctx.builder.CreateCall(strcmpFn, {namePtr, nameStr}, "strcmp_res");
        llvm::Value* isMatch = ctx.builder.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty, 0), "is_match");

        llvm::BasicBlock* callBB = llvm::BasicBlock::Create(ctx.context, "invoke_call_" + cand.name, currentFunc);
        llvm::BasicBlock* nextBB = (ci + 1 < candidates.size())
            ? llvm::BasicBlock::Create(ctx.context, "invoke_next", currentFunc)
            : endBB;

        ctx.builder.CreateCondBr(isMatch, callBB, nextBB);

        // 生成调用块
        ctx.builder.SetInsertPoint(callBB);
        std::vector<llvm::Value*> callArgs;
        callArgs.push_back(thisPtr);

        // 参数类型转换
        auto fnArgIt = cand.func->arg_begin();
        ++fnArgIt; // 跳过 this
        for (size_t ai = 0; ai < extraArgCount; ++ai, ++fnArgIt) {
            llvm::Value* argVal = extraArgs[ai];
            llvm::Type* expectedTy = fnArgIt->getType();
            argVal = castValueToType(argVal, expectedTy, ctx);
            callArgs.push_back(argVal);
        }

        llvm::Value* callResult = ctx.builder.CreateCall(cand.func, callArgs, "invoke_ret");

        // 将结果转为 i32
        llvm::Value* i32Result;
        if (callResult->getType()->isIntegerTy(32)) {
            i32Result = callResult;
        } else if (callResult->getType()->isFloatTy() || callResult->getType()->isDoubleTy()) {
            i32Result = ctx.builder.CreateFPToSI(callResult, i32Ty, "fp_to_i32");
        } else {
            i32Result = llvm::ConstantInt::get(i32Ty, 0);
        }

        ctx.builder.CreateStore(i32Result, resultAlloca);
        ctx.builder.CreateBr(endBB);

        if (nextBB != endBB) {
            ctx.builder.SetInsertPoint(nextBB);
        }
    }

    ctx.builder.SetInsertPoint(endBB);
    return ctx.builder.CreateLoad(i32Ty, resultAlloca, "invoke_result_val");
}
