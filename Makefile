# 设置LLVM的路径（假设安装在默认位置）
LLVM_INCLUDE = /opt/homebrew/Cellar/llvm/20.1.4_1/include
LLVM_LIB = /opt/homebrew/Cellar/llvm/20.1.4_1/lib

CXXFLAGS += -I$(LLVM_INCLUDE)  

# 使用Clang++编译器和Flex、Bison路径
CXX = clang++
FLEX = /opt/homebrew/Cellar/flex/2.6.4_2/bin/flex
BISON = /opt/homebrew/Cellar/bison/3.8.2/bin/bison

CXXFLAGS += -std=c++17 \
			-Wall \
			-I/opt/homebrew/Cellar/flex/2.6.4_2/include \
			-g -O0
LLVMFLAGS = -L$(LLVM_LIB) -lLLVM

all: compiler.out

compiler.out: lexer.o parser.o ast.o symbol.o semanticAnalysis.o main.o 
	$(CXX) $(CXXFLAGS) $(LLVMFLAGS) -o compiler.out lexer.o parser.o ast.o symbol.o semanticAnalysis.o main.o

# Bison - 生成 parser.tab.cpp 和 parser.tab.h
parser.tab.cpp parser.tab.h: parser.y
	$(BISON) -d -t -v -o parser.tab.cpp parser.y

# Flex - 生成 lexer.cpp
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
	rm -f *.o parser.tab.cpp parser.tab.hpp lexer.cpp compiler.out *.bc