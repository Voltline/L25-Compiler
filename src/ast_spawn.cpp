/*
 * SpawnStmt — goroutine-like 并发块的代码生成
 *
 * 工作流程：
 *   1. 扫描 body AST，收集引用的外层变量名
 *   2. 为每个捕获变量在 LLVM struct 中分配一个字段
 *   3. 生成一个 wrapper 函数 __spawn_fn_N(i8* captures)
 *      - 从 capture struct 加载字段到本地 alloca
 *      - 临时重定向 scope 中的 symbol->addr
 *      - 执行 body
 *      - 恢复 symbol->addr
 *      - free(captures)
 *   4. 在调用点 malloc capture struct、填充字段、调用 l25_thread_spawn
 */
#include "ast.h"
#include "codegen_utils.h"
#include "errorReporter.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/DataLayout.h>
#include <set>

// ===== 全局 spawn 计数器 =====
static int spawnCounter = 0;

// ===== AST 遍历：收集所有引用的标识符名 =====
static void collectRefs(const ASTNode* node,
                        std::set<std::string>& refs,
                        std::set<std::string>& locals);

// 辅助：遍历表达式
static void collectRefsExpr(const Expr* e,
                            std::set<std::string>& refs,
                            std::set<std::string>& locals) {
    if (!e) return;
    if (auto* id = dynamic_cast<const IdentExpr*>(e)) {
        if (id->ident != "this" && locals.find(id->ident) == locals.end()) {
            refs.insert(id->ident);
        }
        return;
    }
    if (auto* bin = dynamic_cast<const BinaryExpr*>(e)) {
        collectRefsExpr(bin->lhs.get(), refs, locals);
        collectRefsExpr(bin->rhs.get(), refs, locals);
        return;
    }
    if (auto* un = dynamic_cast<const UnaryExpr*>(e)) {
        collectRefsExpr(un->rhs.get(), refs, locals);
        return;
    }
    if (auto* fc = dynamic_cast<const FuncCallExpr*>(e)) {
        // 函数名本身也是标识符
        if (fc->name) refs.insert(fc->name->ident);
        if (fc->args) {
            for (auto& a : fc->args->args) collectRefsExpr(a.get(), refs, locals);
        }
        return;
    }
    if (auto* mc = dynamic_cast<const MethodCallExpr*>(e)) {
        collectRefsExpr(mc->target.get(), refs, locals);
        if (mc->args) {
            for (auto& a : mc->args->args) collectRefsExpr(a.get(), refs, locals);
        }
        return;
    }
    if (auto* ma = dynamic_cast<const MemberAccessExpr*>(e)) {
        collectRefsExpr(ma->target.get(), refs, locals);
        return;
    }
    if (auto* as = dynamic_cast<const ArraySubscriptExpr*>(e)) {
        if (as->array && locals.find(as->array->ident) == locals.end()) {
            refs.insert(as->array->ident);
        }
        for (auto& s : as->subscript) collectRefsExpr(s.get(), refs, locals);
        return;
    }
    if (auto* sl = dynamic_cast<const StringLiteralExpr*>(e)) {
        (void)sl; return;
    }
    if (auto* ne = dynamic_cast<const NewExpr*>(e)) {
        if (ne->args) {
            for (auto& a : ne->args->args) collectRefsExpr(a.get(), refs, locals);
        }
        return;
    }
    if (auto* na = dynamic_cast<const NewArrayExpr*>(e)) {
        collectRefsExpr(na->sizeExpr.get(), refs, locals);
        return;
    }
    if (auto* addr = dynamic_cast<const AddressOfExpr*>(e)) {
        collectRefsExpr(addr->target.get(), refs, locals);
        return;
    }
    if (auto* deref = dynamic_cast<const DereferenceExpr*>(e)) {
        collectRefsExpr(deref->pointerExpr.get(), refs, locals);
        return;
    }
    if (auto* slen = dynamic_cast<const StrlenExpr*>(e)) {
        collectRefsExpr(slen->target.get(), refs, locals);
        return;
    }
    if (auto* tn = dynamic_cast<const TypenameExpr*>(e)) {
        collectRefsExpr(tn->target.get(), refs, locals);
        return;
    }
    if (auto* inv = dynamic_cast<const InvokeExpr*>(e)) {
        collectRefsExpr(inv->target.get(), refs, locals);
        collectRefsExpr(inv->methodName.get(), refs, locals);
        if (inv->args) {
            for (auto& a : inv->args->args) collectRefsExpr(a.get(), refs, locals);
        }
        return;
    }
    // NumberExpr, FloatNumberExpr, NilExpr — 无引用
}

// 辅助：遍历 BoolExpr
static void collectRefsBool(const BoolExpr* b,
                            std::set<std::string>& refs,
                            std::set<std::string>& locals) {
    if (!b) return;
    collectRefsExpr(b->lhs.get(), refs, locals);
    collectRefsExpr(b->rhs.get(), refs, locals);
    collectRefsBool(b->bool_lhs.get(), refs, locals);
    collectRefsBool(b->bool_rhs.get(), refs, locals);
}

static void collectRefs(const ASTNode* node,
                        std::set<std::string>& refs,
                        std::set<std::string>& locals) {
    if (!node) return;

    if (auto* sl = dynamic_cast<const StmtList*>(node)) {
        for (auto& s : sl->stmts) collectRefs(s.get(), refs, locals);
        return;
    }
    if (auto* ds = dynamic_cast<const DeclareStmt*>(node)) {
        // 先收集 RHS 引用，再注册局部变量
        collectRefsExpr(ds->expr.get(), refs, locals);
        locals.insert(ds->name->ident);
        return;
    }
    if (auto* as = dynamic_cast<const AssignStmt*>(node)) {
        collectRefsExpr(dynamic_cast<const Expr*>(as->name.get()), refs, locals);
        collectRefsExpr(as->expr.get(), refs, locals);
        return;
    }
    if (auto* is = dynamic_cast<const IfStmt*>(node)) {
        collectRefsBool(is->condition.get(), refs, locals);
        collectRefs(is->if_body.get(), refs, locals);
        collectRefs(is->else_body.get(), refs, locals);
        return;
    }
    if (auto* ws = dynamic_cast<const WhileStmt*>(node)) {
        collectRefsBool(ws->condition.get(), refs, locals);
        collectRefs(ws->loop_body.get(), refs, locals);
        return;
    }
    if (auto* fs = dynamic_cast<const ForStmt*>(node)) {
        collectRefs(fs->init.get(), refs, locals);
        collectRefsBool(fs->condition.get(), refs, locals);
        collectRefs(fs->step.get(), refs, locals);
        collectRefs(fs->loop_body.get(), refs, locals);
        return;
    }
    if (auto* fc = dynamic_cast<const FuncCallStmt*>(node)) {
        if (fc->name) refs.insert(fc->name->ident);
        if (fc->args) {
            for (auto& a : fc->args->args) collectRefsExpr(a.get(), refs, locals);
        }
        return;
    }
    if (auto* es = dynamic_cast<const ExprStmt*>(node)) {
        collectRefsExpr(es->expr.get(), refs, locals);
        return;
    }
    if (auto* os = dynamic_cast<const OutputStmt*>(node)) {
        for (auto& e : os->idents) collectRefsExpr(e.get(), refs, locals);
        return;
    }
    if (auto* is = dynamic_cast<const InputStmt*>(node)) {
        for (auto& e : is->idents) collectRefsExpr(e.get(), refs, locals);
        return;
    }
    if (auto* ds = dynamic_cast<const DeleteStmt*>(node)) {
        collectRefsExpr(ds->target.get(), refs, locals);
        return;
    }
    // 嵌套 spawn
    if (auto* sp = dynamic_cast<const SpawnStmt*>(node)) {
        collectRefs(sp->body.get(), refs, locals);
        return;
    }
}

// ===== SpawnStmt 构造与 print =====
SpawnStmt::SpawnStmt(std::unique_ptr<StmtList> body)
    : body(std::move(body)) {}

void SpawnStmt::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Spawn" << std::endl;
    if (body) body->print(indent + 2);
}

// ===== SpawnStmt 代码生成 =====
llvm::Value* SpawnStmt::codeGen(CodeGenContext& ctx) const {
    int spawnId = spawnCounter++;

    // ------ 1. 收集捕获变量 ------
    std::set<std::string> refs, locals;
    collectRefs(body.get(), refs, locals);

    // 在当前 scope 解析引用，过滤掉函数、类型等
    struct CaptureInfo {
        std::string name;
        SymbolInfo* symbol;
        TypeInfo    typeInfo;
        llvm::Type* llvmType;
    };
    std::vector<CaptureInfo> captures;

    for (const auto& name : refs) {
        SymbolInfo* sym = scope->lookup(name);
        if (!sym) continue;
        // 跳过函数定义（它们是全局的，不需要捕获）
        if (sym->kind == SymbolKind::Function) continue;
        if (sym->kind == SymbolKind::Program) continue;
        if (sym->kind == SymbolKind::Invalid) continue;
        if (!sym->addr) continue;  // 无地址的符号无需捕获

        TypeInfo ti = typeInfoFromSymbol(sym);
        llvm::Type* ty = typeInfoToLLVMValueType(ti, ctx.context);
        if (!ty) continue;

        captures.push_back({name, sym, ti, ty});
    }

    // ------ 2. 创建 capture struct 类型 ------
    std::string structName = "__spawn_capture_" + std::to_string(spawnId);
    std::vector<llvm::Type*> fieldTypes;
    for (auto& cap : captures) {
        fieldTypes.push_back(cap.llvmType);
    }
    llvm::StructType* capStructTy = llvm::StructType::create(ctx.context, fieldTypes, structName);

    // ------ 3. 生成 wrapper 函数 ------
    std::string wrapperName = "__spawn_fn_" + std::to_string(spawnId);
    auto* i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(ctx.context), 0);
    auto* voidTy  = llvm::Type::getVoidTy(ctx.context);

    llvm::FunctionType* wrapperFnTy = llvm::FunctionType::get(voidTy, {i8PtrTy}, false);
    llvm::Function* wrapperFn = llvm::Function::Create(
        wrapperFnTy, llvm::Function::InternalLinkage, wrapperName, ctx.module);

    // 保存当前 IR 生成位置
    llvm::IRBuilder<>::InsertPoint savedIP = ctx.builder.saveIP();
    llvm::Function* savedFunc = ctx.currentFunction;
    auto savedCleanupStack = std::move(ctx.cleanupStack);
    ctx.cleanupStack.clear();

    // 设置 wrapper 函数入口
    llvm::BasicBlock* wrapperEntry = llvm::BasicBlock::Create(ctx.context, "entry", wrapperFn);
    ctx.builder.SetInsertPoint(wrapperEntry);
    ctx.currentFunction = wrapperFn;

    // 注册当前 spawn 线程到 GC
    ensureGCRuntimeDeclared(ctx);
    auto* threadInitFn = ctx.module.getFunction("l25_gc_thread_init");
    ctx.builder.CreateCall(threadInitFn, {});

    // 解包 capture struct
    llvm::Value* rawArg = wrapperFn->getArg(0);
    llvm::Value* capPtr = ctx.builder.CreateBitCast(rawArg,
        llvm::PointerType::get(capStructTy, 0), "cap.ptr");

    // 为每个捕获变量创建本地 alloca 并从 struct 加载
    std::vector<std::pair<SymbolInfo*, llvm::Value*>> savedAddrs; // 保存原始 addr

    ctx.pushCleanupScope();

    for (size_t i = 0; i < captures.size(); i++) {
        auto& cap = captures[i];
        llvm::Value* fieldPtr = ctx.builder.CreateStructGEP(capStructTy, capPtr, i,
            "cap." + cap.name + ".ptr");
        llvm::Value* fieldVal = ctx.builder.CreateLoad(cap.llvmType, fieldPtr,
            "cap." + cap.name);

        // 创建本地 alloca
        llvm::AllocaInst* localAlloca = ctx.builder.CreateAlloca(cap.llvmType, nullptr,
            cap.name + ".local");
        ctx.builder.CreateStore(fieldVal, localAlloca);

        // 重定向 symbol->addr 到本地 alloca
        savedAddrs.push_back({cap.symbol, cap.symbol->addr});
        cap.symbol->addr = localAlloca;

        // 注册 RAII 清理（仅对 spawn 内部新拥有的资源）
        // 字符串深拷贝后需要清理
        if (cap.typeInfo.kind == SymbolKind::String && cap.typeInfo.pointerLevel == 0) {
            ctx.registerCleanup(localAlloca, CleanupKind::String);
        }
        // Channel 不在 spawn 内清理（由外层 scope 管理）
    }

    // free(captures)
    ensureStringRuntimeDeclared(ctx);  // 确保 free 已声明
    auto* freeFn = ctx.module.getFunction("free");
    ctx.builder.CreateCall(freeFn, {rawArg});

    // ------ 4. 生成 body 代码 ------
    body->codeGen(ctx);

    // ------ 5. RAII 清理 + return void ------
    emitScopeCleanup(ctx);

    // 注销当前 spawn 线程的 GC 根栈
    auto* threadFiniFn = ctx.module.getFunction("l25_gc_thread_fini");
    ctx.builder.CreateCall(threadFiniFn, {});

    ctx.builder.CreateRetVoid();

    // ------ 6. 恢复 symbol->addr 和 IR 生成位置 ------
    for (auto& [sym, origAddr] : savedAddrs) {
        sym->addr = origAddr;
    }

    ctx.cleanupStack = std::move(savedCleanupStack);
    ctx.builder.restoreIP(savedIP);
    ctx.currentFunction = savedFunc;

    // ------ 7. 在调用点：malloc capture struct、填充、调用 l25_thread_spawn ------

    // 声明 l25_thread_spawn
    if (!ctx.module.getFunction("l25_thread_spawn")) {
        // void l25_thread_spawn(void (*fn)(void*), void* arg)
        auto* fnPtrTy = llvm::PointerType::get(
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false), 0);
        ctx.module.getOrInsertFunction("l25_thread_spawn",
            llvm::FunctionType::get(voidTy, {fnPtrTy, i8PtrTy}, false));
    }

    // malloc
    uint64_t structSize = ctx.module.getDataLayout().getTypeAllocSize(capStructTy);
    auto* mallocFn = ctx.module.getFunction("malloc");
    llvm::Value* rawCap = ctx.builder.CreateCall(mallocFn,
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.context), structSize)},
        "spawn.cap");
    llvm::Value* typedCap = ctx.builder.CreateBitCast(rawCap,
        llvm::PointerType::get(capStructTy, 0), "spawn.cap.typed");

    // 填充捕获字段
    for (size_t i = 0; i < captures.size(); i++) {
        auto& cap = captures[i];
        llvm::Value* val = ctx.builder.CreateLoad(cap.llvmType, cap.symbol->addr,
            "spawn.load." + cap.name);

        // 字符串需要深拷贝（spawn 块独立拥有一份）
        if (cap.typeInfo.kind == SymbolKind::String && cap.typeInfo.pointerLevel == 0) {
            val = emitStringDeepCopy(val, ctx);
        }

        llvm::Value* fieldPtr = ctx.builder.CreateStructGEP(capStructTy, typedCap, i,
            "spawn.store." + cap.name);
        ctx.builder.CreateStore(val, fieldPtr);
    }

    // 调用 l25_thread_spawn(wrapperFn, rawCap)
    auto* spawnFn = ctx.module.getFunction("l25_thread_spawn");
    llvm::Value* fnPtr = ctx.builder.CreateBitCast(wrapperFn,
        llvm::PointerType::get(
            llvm::FunctionType::get(voidTy, {i8PtrTy}, false), 0));
    ctx.builder.CreateCall(spawnFn, {fnPtr, rawCap});

    return nullptr;
}
