#pragma once
#include <string>

struct ASTNode;

// 加载源代码文件，供错误输出时显示上下文
bool loadSourceFile(const std::string& filename);

// 使用行列号或 AST 节点报告错误，自动输出源码片段与插入符号
void reportErrorAt(const std::string& category, int lineno, int column, const std::string& msg);
void reportErrorAt(const ASTNode& node, const std::string& category, const std::string& msg);

// 警告输出，不会中断编译
void reportWarningAt(const std::string& category, int lineno, int column, const std::string& msg);
void reportWarningAt(const ASTNode& node, const std::string& category, const std::string& msg);

// 当前源文件名，若尚未设置则为空
const std::string& currentSourceFile();
