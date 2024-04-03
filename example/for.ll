define i32 @main() {
entry:
	%s = alloca i32, align 4
	store i32 0, i32* %s, align 4
	%a = alloca i32, align 4
	store i32 1, i32* %a, align 4
	%index.start = load i32, i32* %a, align 4
	br label %loop
loop:
	%index = phi i32 [ %index.start, %entry ], [ %next_index, %body ]
	%cmptmp0 = icmp ult i32 %index, 5
	br i1 %cmptmp0, label %body, label %exit
body:
	%s1 = load i32, i32* %s, align 4
	%addtmp0 = add i32 %s1, 2
	store i32 %addtmp0, i32* %s, align 4
	%addtmp1 = add i32 %index, 1
	%next_index = add i32 %addtmp1, 0
	br label %loop
exit:
	%s0 = load i32, i32* %s, align 4
	ret i32 %s0
}

