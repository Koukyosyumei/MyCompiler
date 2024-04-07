define i32 @main() {
entry:
	%cmptmp0 = icmp ult i32 4, 3
	br i1 %cmptmp0, label %then0, label %else0

then0:
	br label %ifcont0

else0:
	br label %ifcont0

ifcont0:
	%iftmp0 = phi i32 [ 12, %then0 ], [ 13, %else0 ]
	ret i32 %iftmp0
}

