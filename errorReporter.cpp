#include "include/errorReporter.h"
#include "include/ast.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>

extern bool hasError;

namespace {

std::string gCurrentFile;
std::vector<std::string> gSourceLines;

void printSourceContext(int lineno, int column)
{
    if (lineno <= 0 || lineno > static_cast<int>(gSourceLines.size()))
        return;

    const std::string& line = gSourceLines[lineno - 1];
    std::cerr << "  " << line << '\n' << "  ";

    int caretColumn = std::max(column, 1);
    for (int i = 1; i < caretColumn; ++i) {
        char ch = (i - 1 < static_cast<int>(line.size())) ? line[i - 1] : ' ';
        std::cerr << (ch == '\t' ? '\t' : ' ');
    }
    std::cerr << "^" << std::endl;
}

} // namespace

bool loadSourceFile(const std::string& filename)
{
    gCurrentFile = filename;
    gSourceLines.clear();

    std::ifstream in(filename);
    if (!in.is_open()) {
        std::cerr << "无法读取源文件: " << filename << std::endl;
        return false;
    }

    std::string line;
    while (std::getline(in, line)) {
        gSourceLines.push_back(line);
    }
    return true;
}

const std::string& currentSourceFile()
{
    return gCurrentFile;
}

void reportErrorAt(const std::string& category, int lineno, int column, const std::string& msg)
{
    std::string filename = gCurrentFile.empty() ? std::string{"<未知文件>"} : gCurrentFile;
    int safeLine = std::max(lineno, 1);
    int safeColumn = std::max(column, 1);

    std::cerr << filename << ':' << safeLine << ':' << safeColumn << ": "
              << "\033[1;31merror\033[0m: [" << category << "] " << msg << std::endl;
    printSourceContext(safeLine, safeColumn);
    hasError = true;
}

void reportErrorAt(const ASTNode& node, const std::string& category, const std::string& msg)
{
    reportErrorAt(category, node.lineno, node.column, msg);
}

void reportWarningAt(const std::string& category, int lineno, int column, const std::string& msg)
{
    std::string filename = gCurrentFile.empty() ? std::string{"<未知文件>"} : gCurrentFile;
    int safeLine = std::max(lineno, 1);
    int safeColumn = std::max(column, 1);

    std::cerr << filename << ':' << safeLine << ':' << safeColumn << ": "
              << "\033[1;33mwarning\033[0m: [" << category << "] " << msg << std::endl;
    printSourceContext(safeLine, safeColumn);
}

void reportWarningAt(const ASTNode& node, const std::string& category, const std::string& msg)
{
    reportWarningAt(category, node.lineno, node.column, msg);
}

