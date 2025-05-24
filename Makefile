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
LLVM_CONFIG = llvm-config
LLVM_CXXFLAGS_RAW = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CXXFLAGS = $(filter-out -std=%,$(LLVM_CXXFLAGS_RAW))
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs)

# 编译器
CXX = clang++

# 基础编译选项
BASE_CXXFLAGS = -std=c++20 -Wall $(LLVM_CXXFLAGS) -I$(FLEX_INCLUDE)

# 默认编译选项
CXXFLAGS ?= $(BASE_CXXFLAGS)

# 默认目标
all: l25cc

# debug 目标，附加调试和地址消毒器选项
debug: CXXFLAGS += -g -fsanitize=address -fno-omit-frame-pointer
debug: l25cc

l25cc: lexer.o parser.o ast.o symbol.o semanticAnalysis.o main.o 
	$(CXX) $(CXXFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) -o l25cc lexer.o parser.o ast.o symbol.o semanticAnalysis.o main.o

parser.tab.cpp parser.tab.h: parser.y
	$(BISON) -d -t -v -o parser.tab.cpp parser.y

lexer.cpp: lexer.l parser.tab.h
	$(FLEX) --nounput -o lexer.cpp lexer.l 

lexer.o: lexer.cpp
	$(CXX) $(CXXFLAGS) -c lexer.cpp

parser.o: parser.tab.cpp
	$(CXX) $(CXXFLAGS) -c parser.tab.cpp -o parser.o

ast.o: ast.cpp include/ast.h
	$(CXX) $(CXXFLAGS) -c ast.cpp

symbol.o: symbol.cpp
	$(CXX) $(CXXFLAGS) -c symbol.cpp

semanticAnalysis.o: semanticAnalysis.cpp
	$(CXX) $(CXXFLAGS) -c semanticAnalysis.cpp

main.o: main.cpp
	$(CXX) $(CXXFLAGS) -c main.cpp

clean:
	rm -f *.o parser.tab.cpp parser.tab.hpp lexer.cpp compiler.out *.bc l25cc parser.output

.PHONY: all clean debug