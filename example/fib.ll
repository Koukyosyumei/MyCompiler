define i32 @fib(i32 %x) {
entry:
	%cmptmp0 = icmp ult i32 %x, 3
	br i1 %cmptmp0, label %then0, label %else0

then0:
	br label %ifcont0

else0:
	%subtmp0 = sub i32 %x, 1
	%calltmp0 = call i32 @fib(i32 %subtmp0)
	%subtmp1 = sub i32 %x, 2
	%calltmp1 = call i32 @fib(i32 %subtmp1)
	%addtmp0 = add i32 %calltmp0, %calltmp1
	br label %ifcont0

ifcont0:
	%iftmp0 = phi i32 [ 1, %then0 ], [ %addtmp0, %else0 ]
	ret i32 %iftmp0
}
define i32 @main() {
entry:
	%calltmp2 = call i32 @fib(i32 10)
	ret i32 %calltmp2
}

