cmake_minimum_required(VERSION 3.16)
project(L25_Compiler)

set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})

set(SOURCE_FILES
    main.cpp
    #lexer.cpp
    #parser.cpp
)

# 添加可执行文件并包含所有源文件
add_executable(${PROJECT_NAME} ${SOURCE_FILES})

llvm_map_components_to_libnames(LLVM_LIBS core orcjit native)
target_link_libraries(L25_Compiler ${LLVM_LIBS})