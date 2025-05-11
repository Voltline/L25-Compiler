#include "symbol.h"

/* Scope 方法定义 */
// 构造函数
Scope::Scope(Scope* parent) : parent(parent) {}
// 本地及上层
SymbolInfo* Scope::lookup(const std::string& name)
{
    auto it{ table.find(name) };
    if (it != table.end()) return &it->second;
    if (parent) return parent->lookup(name);
    return nullptr;
}

SymbolInfo* Scope::lookupLocal(const std::string& name) {
    auto it{ table.find(name) };
    return it != table.end() ? &it->second : nullptr;
}

bool Scope::declare(const std::string& name, const SymbolInfo& info)
{
    return table.emplace(name, info).second;
}

Scope* Scope::createChild()
{
    // 写法等价于下面的语句
    // std::unique_ptr<Scope>(new Scope(this));
    children.emplace_back(std::make_unique<Scope>(this));
    return children.back().get();
}

Scope* Scope::getParent() const
{
    return parent;
}

void Scope::print(int depth) const {
    std::string indent(depth, ' ');  // 通过空格缩进表示层级
    std::cout << indent << "Scope {" << std::endl;

    for (const auto& pair : table) {
        const SymbolInfo& symbol = pair.second;
        std::cout << indent << "  " << symbol.name << " : " << SymbolName[static_cast<int>(symbol.kind)] << std::endl;
    }

    // 打印所有子作用域
    for (const auto& child : children) {
        child->print(depth + 2);
    }

    std::cout << indent << "}" << std::endl;
}