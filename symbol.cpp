#include "symbol.h"

/* SymbolTable 方法定义 */
// 加入符号表方法
bool SymbolTable::declare(const std::string& name, const SymbolInfo& info)
{
    // 使用emplace方法原地构造
    return table.emplace(name, info).second;
}

// 查询符号表方法
SymbolInfo* SymbolTable::lookup(const std::string& name)
{
    auto it{ table.find(name) };
    if (it != table.end()) {
        return &it->second;
    } else {
        return nullptr;
    }
}

/* SymbolTableManager 方法定义 */

// 进入下一层作用域方法
void SymbolTableManager::enterScope()
{
    // 直接加入下一层作用域符号表
    scopes.emplace_back();
}

// 离开当前作用域方法
void SymbolTableManager::exitScope()
{
    // 销毁当前层符号表
    if (!scopes.empty()) {
        scopes.pop_back();
    }
}

// 加入符号表方法(仅限当前作用域)
bool SymbolTableManager::declare(const std::string& name, const SymbolInfo& info)
{
    // 在当前作用域插入一条符号
    return scopes.back().declare(name, info);
}

// 查询符号表方法(在当前及上层作用域)
SymbolInfo* SymbolTableManager::lookup(const std::string& name)
{
    // 栈结构下，从尾往前找
    for (auto it{ scopes.rbegin() }; it != scopes.rend(); it++) {
        auto* info{ it->lookup(name) };
        if (info) {
            return info;
        }
    }
    return nullptr;
}

// 同层查询符号表方法(仅在当前作用域)
SymbolInfo* SymbolTableManager::lookupInplace(const std::string& name)
{  
    return scopes.back().lookup(name);   
}