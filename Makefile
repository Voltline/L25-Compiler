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

# 基础编译选项
BASE_CXXFLAGS = -std=c++20 -Wall $(LLVM_CXXFLAGS) -I$(FLEX_INCLUDE)

# 默认编译选项
CXXFLAGS ?= $(BASE_CXXFLAGS)

# 默认目标
all: l25cc

# debug 目标，附加调试和地址消毒器选项
debug: CXXFLAGS += -g -fsanitize=address -fno-omit-frame-pointer
debug: l25cc

# test 目标
test: all
	./test.sh

# AST 拆分后的目标文件
AST_OBJS = codegen_utils.o ast_node.o ast_class.o ast_func.o ast_stmt.o ast_expr.o ast_string.o ast_reflect.o
AST_HEADERS = include/ast.h include/codegen_utils.h

l25cc: lexer.o parser.o $(AST_OBJS) symbol.o semanticAnalysis.o errorReporter.o main.o
	$(CXX) $(CXXFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) -o l25cc lexer.o parser.o $(AST_OBJS) symbol.o semanticAnalysis.o errorReporter.o main.o

parser.tab.cpp parser.tab.h: parser.y
	$(BISON) -d -t -v -o parser.tab.cpp parser.y

lexer.cpp: lexer.l parser.tab.h
	$(FLEX) --nounput -o lexer.cpp lexer.l 

lexer.o: lexer.cpp
	$(CXX) $(CXXFLAGS) -c lexer.cpp

parser.o: parser.tab.cpp
	$(CXX) $(CXXFLAGS) -c parser.tab.cpp -o parser.o

codegen_utils.o: codegen_utils.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c codegen_utils.cpp

ast_node.o: ast_node.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c ast_node.cpp

ast_class.o: ast_class.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c ast_class.cpp

ast_func.o: ast_func.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c ast_func.cpp

ast_stmt.o: ast_stmt.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c ast_stmt.cpp

ast_expr.o: ast_expr.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c ast_expr.cpp

ast_string.o: ast_string.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c ast_string.cpp

ast_reflect.o: ast_reflect.cpp $(AST_HEADERS)
	$(CXX) $(CXXFLAGS) -c ast_reflect.cpp

symbol.o: symbol.cpp
	$(CXX) $(CXXFLAGS) -c symbol.cpp

semanticAnalysis.o: semanticAnalysis.cpp
	$(CXX) $(CXXFLAGS) -c semanticAnalysis.cpp

errorReporter.o: errorReporter.cpp include/errorReporter.h
	$(CXX) $(CXXFLAGS) -c errorReporter.cpp

main.o: main.cpp
	$(CXX) $(CXXFLAGS) -c main.cpp

clean:
	rm -f *.o parser.tab.cpp parser.tab.hpp lexer.cpp compiler.out *.bc l25cc parser.output

.PHONY: all clean debug test
