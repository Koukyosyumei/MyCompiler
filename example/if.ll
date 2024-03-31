define i32 @main() {
entry:
	%cmptmp0 = icmp ult i32 4, 3
	br i1 %cmptmp0, label %then, label %else

then:
	br label %ifcont

else:
	br label %ifcont

ifcont:
	%iftmp = phi i32 [ 12, %then ], [ 13, %else ]
	ret i32 %iftmp
}

