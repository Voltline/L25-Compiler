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
  br label %while.cond

while.cond:                                       ; preds = %while.after4, %entry
  %a1 = load i32, ptr %a, align 4
  %cmplt = icmp slt i32 %a1, 100
  br i1 %cmplt, label %while.body, label %while.after

while.body:                                       ; preds = %while.cond
  br label %while.cond2

while.after:                                      ; preds = %while.cond
  %array_elem11 = getelementptr [3 x [4 x [5 x i32]]], ptr %b, i32 0, i32 1, i32 2, i32 3
  %load_elem = load i32, ptr %array_elem11, align 4
  %1 = call i32 (ptr, ...) @printf(ptr @2, i32 %load_elem)
  %2 = call i32 (ptr, ...) @printf(ptr @3, i32 33)
  %3 = call i32 (ptr, ...) @printf(ptr @3, i32 66)
  ret i32 0

while.cond2:                                      ; preds = %while.body3, %while.body
  %a5 = load i32, ptr %a, align 4
  %cmplt6 = icmp slt i32 %a5, 50
  br i1 %cmplt6, label %while.body3, label %while.after4

while.body3:                                      ; preds = %while.cond2
  %a7 = load i32, ptr %a, align 4
  %addtmp = add i32 %a7, 10
  store i32 %addtmp, ptr %a, align 4
  br label %while.cond2

while.after4:                                     ; preds = %while.cond2
  %a8 = load i32, ptr %a, align 4
  %addtmp9 = add i32 %a8, 1
  store i32 %addtmp9, ptr %a, align 4
  %a10 = load i32, ptr %a, align 4
  %4 = call i32 (ptr, ...) @printf(ptr @1, i32 %a10)
  br label %while.cond
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #0

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: write) }