# 自动检测平台，配置 FLEX 和 BISON 路径
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
  # macOS 使用 Homebrew 安装的 flex 和 bison
  BREW_PREFIX := $(shell brew --prefix)
  FLEX := $(BREW_PREFIX)/opt/flex/bin/flex
  BISON := $(BREW_PREFIX)/opt/bison/bin/bison
  FLEX_INCLUDE := $(BREW_PREFIX)/opt/flex/include
else
  # Linux 或其他系统直接用系统自带的
  FLEX := flex
  BISON := bison
  FLEX_INCLUDE := /usr/include
endif

# 使用 llvm-config 自动获取 LLVM 编译参数，并过滤掉 -std=
LLVM_CONFIG = llvm-config-18
LLVM_CXXFLAGS_RAW = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CXXFLAGS = $(filter-out -std=%,$(LLVM_CXXFLAGS_RAW))
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs)

# 编译器
CXX = clang++-18

# 目录结构
SRCDIR = src
INCDIR = include

# 基础编译选项（-Iinclude 查找头文件，-I. 查找生成的 parser.tab.hpp）
BASE_CXXFLAGS = -std=c++20 -Wall $(LLVM_CXXFLAGS) -I$(FLEX_INCLUDE) -I$(INCDIR) -I.

# 默认编译选项
CXXFLAGS ?= $(BASE_CXXFLAGS)

# 默认目标
all: l25cc libl25rt.a

# debug 目标，附加调试和地址消毒器选项
debug: CXXFLAGS += -g -fsanitize=address -fno-omit-frame-pointer
debug: l25cc

# test 目标
test: all
	./test.sh

# AST 拆分后的目标文件
AST_OBJS = codegen_utils.o ast_node.o ast_class.o ast_func.o ast_stmt.o ast_expr.o ast_string.o ast_reflect.o
AST_HEADERS = $(INCDIR)/ast.h $(INCDIR)/codegen_utils.h

# 链接
l25cc: lexer.o parser.o $(AST_OBJS) symbol.o semanticAnalysis.o errorReporter.o main.o
	$(CXX) $(CXXFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) -o l25cc lexer.o parser.o $(AST_OBJS) symbol.o semanticAnalysis.o errorReporter.o main.o

# Bison / Flex 生成规则（生成文件留在根目录）
parser.tab.cpp parser.tab.hpp: $(SRCDIR)/parser.y
	$(BISON) -d -t -v -o parser.tab.cpp $(SRCDIR)/parser.y

lexer.cpp: $(SRCDIR)/lexer.l parser.tab.hpp
	$(FLEX) --nounput -o lexer.cpp $(SRCDIR)/lexer.l

# 生成文件编译
lexer.o: lexer.cpp
	$(CXX) $(CXXFLAGS) -c lexer.cpp

parser.o: parser.tab.cpp
	$(CXX) $(CXXFLAGS) -c parser.tab.cpp -o parser.o

# AST 各模块编译
codegen_utils.o: $(SRCDIR)/codegen_utils.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/codegen_utils.cpp

ast_node.o: $(SRCDIR)/ast_node.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/ast_node.cpp

ast_class.o: $(SRCDIR)/ast_class.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/ast_class.cpp

ast_func.o: $(SRCDIR)/ast_func.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/ast_func.cpp

ast_stmt.o: $(SRCDIR)/ast_stmt.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/ast_stmt.cpp

ast_expr.o: $(SRCDIR)/ast_expr.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/ast_expr.cpp

ast_string.o: $(SRCDIR)/ast_string.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/ast_string.cpp

ast_reflect.o: $(SRCDIR)/ast_reflect.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/ast_reflect.cpp

# 其他模块编译
symbol.o: $(SRCDIR)/symbol.cpp $(INCDIR)/symbol.h
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/symbol.cpp

semanticAnalysis.o: $(SRCDIR)/semanticAnalysis.cpp $(INCDIR)/semanticAnalysis.h
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/semanticAnalysis.cpp

errorReporter.o: $(SRCDIR)/errorReporter.cpp $(INCDIR)/errorReporter.h
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/errorReporter.cpp

main.o: $(SRCDIR)/main.cpp
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/main.cpp

# ===== 运行时库 =====
RUNTIME_DIR = runtime
CC = clang-18

l25_vector.o: $(RUNTIME_DIR)/l25_vector.c $(RUNTIME_DIR)/l25_runtime.h
	$(CC) -O2 -c $(RUNTIME_DIR)/l25_vector.c -I$(RUNTIME_DIR) -o l25_vector.o

l25_map.o: $(RUNTIME_DIR)/l25_map.c $(RUNTIME_DIR)/l25_runtime.h
	$(CC) -O2 -c $(RUNTIME_DIR)/l25_map.c -I$(RUNTIME_DIR) -o l25_map.o

libl25rt.a: l25_vector.o l25_map.o
	ar rcs libl25rt.a l25_vector.o l25_map.o

clean:
	rm -f *.o parser.tab.cpp parser.tab.hpp lexer.cpp compiler.out *.bc l25cc parser.output libl25rt.a

.PHONY: all clean debug test
