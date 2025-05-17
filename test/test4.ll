; ModuleID = 'L25'
source_filename = "L25"

@0 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

declare i32 @scanf(ptr, ...)

define i32 @fibs(i32 %i) {
entry:
  %i1 = alloca i32, align 4
  store i32 %i, ptr %i1, align 4
  %res = alloca i32, align 4
  store i32 1, ptr %res, align 4
  %i2 = load i32, ptr %i1, align 4
  %cmpgt = icmp sgt i32 %i2, 2
  br i1 %cmpgt, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  %i3 = load i32, ptr %i1, align 4
  %subtmp = sub i32 %i3, 1
  %fibs_call = call i32 @fibs(i32 %subtmp)
  %i4 = load i32, ptr %i1, align 4
  %subtmp5 = sub i32 %i4, 2
  %fibs_call6 = call i32 @fibs(i32 %subtmp5)
  %addtmp = add i32 %fibs_call, %fibs_call6
  store i32 %addtmp, ptr %res, align 4
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  %res7 = load i32, ptr %res, align 4
  ret i32 %res7
}

define i32 @main() {
entry:
  %a = alloca i32, align 4
  %0 = call i32 (ptr, ...) @scanf(ptr @0, ptr %a)
  %i = alloca i32, align 4
  store i32 1, ptr %i, align 4
  br label %while.cond

while.cond:                                       ; preds = %while.body, %entry
  %i1 = load i32, ptr %i, align 4
  %a2 = load i32, ptr %a, align 4
  %cmplt = icmp slt i32 %i1, %a2
  br i1 %cmplt, label %while.body, label %while.after

while.body:                                       ; preds = %while.cond
  %i3 = load i32, ptr %i, align 4
  %fibs_call = call i32 @fibs(i32 %i3)
  %1 = call i32 (ptr, ...) @printf(ptr @1, i32 %fibs_call)
  %i4 = load i32, ptr %i, align 4
  %addtmp = add i32 %i4, 1
  store i32 %addtmp, ptr %i, align 4
  br label %while.cond

while.after:                                      ; preds = %while.cond
  ret i32 0
}