#!/bin/bash
# RAII 内存泄漏测试脚本
# 用 valgrind 检测每个 RAII 测试用例

set -e

COMPILER="./l25cc"
TESTS=(
    "test/test_raii_string.l25"
    "test/test_raii_class.l25"
    "test/test_raii_func.l25"
    "test/test_raii_linked_list.l25"
    "test/test17.l25"
    "test/test18.l25"
    "test/test20.l25"
    "test/test_string.l25"
)

PASS=0
FAIL=0

for src in "${TESTS[@]}"; do
    name=$(basename "$src" .l25)
    ll="/tmp/${name}.ll"
    bin="/tmp/${name}_bin"

    echo "========================================"
    echo "== $src =="

    # 编译到 LLVM IR
    if ! $COMPILER "$src" -emit-ir -o "$ll" 2>&1; then
        echo "  [SKIP] 编译失败"
        FAIL=$((FAIL + 1))
        continue
    fi

    # 编译到可执行文件
    if ! clang-18 "$ll" -o "$bin" -lm 2>&1; then
        echo "  [SKIP] clang 链接失败"
        FAIL=$((FAIL + 1))
        continue
    fi

    # 用 valgrind 运行
    echo "--- 程序输出 ---"
    VG_OUTPUT=$(valgrind --leak-check=full --show-leak-kinds=all --error-exitcode=99 "$bin" 2>&1)
    VG_EXIT=$?

    # 提取程序输出（非 valgrind 行）
    echo "$VG_OUTPUT" | grep -v "^==" || true

    # 提取 valgrind 摘要
    echo "--- valgrind 摘要 ---"
    echo "$VG_OUTPUT" | grep -E "(definitely|indirectly|possibly|still reachable|LEAK SUMMARY|ERROR SUMMARY|total heap)" || true

    if echo "$VG_OUTPUT" | grep -q "definitely lost: 0 bytes"; then
        if echo "$VG_OUTPUT" | grep -q "indirectly lost: 0 bytes"; then
            echo "  [PASS] 无确定性泄漏"
            PASS=$((PASS + 1))
        else
            echo "  [LEAK] 存在间接泄漏"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "  [LEAK] 存在确定性泄漏!"
        FAIL=$((FAIL + 1))
    fi
    echo ""
done

echo "========================================"
echo "结果: $PASS 通过, $FAIL 失败 (共 ${#TESTS[@]} 个)"
