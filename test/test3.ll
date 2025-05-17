@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

declare i32 @scanf(ptr, ...)

define i32 @tt(i32 %a, ptr %d) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  %array_elem = getelementptr [3 x [4 x [5 x i32]]], ptr %d, i32 0, i32 1, i32 2, i32 3
  store i32 31241, ptr %array_elem, align 4
  %array_elem2 = getelementptr [3 x [4 x [5 x i32]]], ptr %d, i32 0, i32 1, i32 2, i32 3
  %load_elem = load i32, ptr %array_elem2, align 4
  %aaa_call = call i32 @aaa()
  %addtmp = add i32 %load_elem, %aaa_call
  ret i32 %addtmp
}

define i32 @aaa() {
entry:
  %a = alloca i32, align 4
  store i32 1, ptr %a, align 4
  ret i32 1
}

define i32 @main() {
entry:
  %a = alloca i32, align 4
  %b = alloca [3 x [4 x [5 x i32]]], align 4
  call void @llvm.memset.p0.i64(ptr %b, i8 0, i64 240, i1 false)
  %0 = call i32 @tt(i32 1, ptr %b)
  %array_elem = getelementptr [3 x [4 x [5 x i32]]], ptr %b, i32 0, i32 1, i32 2, i32 3
  %load_elem = load i32, ptr %array_elem, align 4
  %1 = call i32 (ptr, ...) @printf(ptr @0, i32 %load_elem)
  ret i32 0
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #0

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: write) }