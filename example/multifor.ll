define i32 @main() {
entry:
	%s = alloca i32, align 4
	store i32 0, i32* %s, align 4
	%t = alloca i32, align 4
	store i32 1, i32* %t, align 4
	%a = alloca i32, align 4
	store i32 1, i32* %a, align 4
	%initial_index0 = load i32, i32* %a, align 4
	br label %loop0
loop0:
	%index0 = phi i32 [ %initial_index0, %entry ], [ %next_index0, %body0 ]
	%cmptmp0 = icmp ult i32 %index0, 5
	br i1 %cmptmp0, label %body0, label %exit_loop0
body0:
	%s1 = load i32, i32* %s, align 4
	%addtmp0 = add i32 %s1, 3
	store i32 %addtmp0, i32* %s, align 4
	%addtmp1 = add i32 %index0, 1
	%next_index0 = add i32 %addtmp1, 0
	br label %loop0
exit_loop0:
	%b = alloca i32, align 4
	store i32 1, i32* %b, align 4
	%initial_index1 = load i32, i32* %b, align 4
	br label %loop1
loop1:
	%index1 = phi i32 [ %initial_index1, %exit_loop0 ], [ %next_index1, %body1 ]
	%cmptmp1 = icmp ult i32 %index1, 5
	br i1 %cmptmp1, label %body1, label %exit_loop1
body1:
	%t1 = load i32, i32* %t, align 4
	%addtmp2 = add i32 %t1, 2
	store i32 %addtmp2, i32* %t, align 4
	%addtmp3 = add i32 %index1, 1
	%next_index1 = add i32 %addtmp3, 0
	br label %loop1
exit_loop1:
	%s0 = load i32, i32* %s, align 4
	%t0 = load i32, i32* %t, align 4
	%addtmp4 = add i32 %s0, %t0
	ret i32 %addtmp4
}

