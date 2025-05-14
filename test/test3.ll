@0 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@2 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@3 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

declare i32 @scanf(ptr, ...)

define i32 @main() {
entry:
  %a = alloca i32, align 4
  %b = alloca [3 x [4 x [5 x i32]]], align 4
  call void @llvm.memset.p0.i64(ptr %b, i8 0, i64 240, i1 false)
  %0 = call i32 (ptr, ...) @scanf(ptr @0, ptr %a)
  %array_elem = getelementptr [3 x [4 x [5 x i32]]], ptr %b, i32 0, i32 1, i32 2, i32 3
  store i32 1, ptr %array_elem, align 4
  %a1 = load i32, ptr %a, align 4
  %cmpgt = icmp sgt i32 %a1, 3
  br i1 %cmpgt, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  store i32 12345, ptr %a, align 4
  br label %if.end

if.else:                                          ; preds = %entry
  store i32 23333, ptr %a, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %array_elem2 = getelementptr [3 x [4 x [5 x i32]]], ptr %b, i32 0, i32 1, i32 2, i32 3
  %load_elem = load i32, ptr %array_elem2, align 4
  %1 = call i32 (ptr, ...) @printf(ptr @1, i32 %load_elem)
  %a3 = load i32, ptr %a, align 4
  %2 = call i32 (ptr, ...) @printf(ptr @2, i32 %a3)
  %3 = call i32 (ptr, ...) @printf(ptr @3, i32 33)
  %4 = call i32 (ptr, ...) @printf(ptr @3, i32 66)
  ret i32 0
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #0

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: write) }