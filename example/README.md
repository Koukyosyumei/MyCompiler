# Examples

This folder provides some LLVM-IR codes generated from Haleidoscope.

- [/example/if.ll](/example/if.ll)

```bash
./build/hal "def main() if 4<3 then 12 else 13;"
```

- [/example/fib.ll](/example/fib.ll)

```bash
./build/hal "def fib(x) if x<3 then 1 else fib(x-1)+fib(x-2); def main() fib(10);"
```

- [/example/for.ll](/example/for.ll)

```bash
./build/hal "def main() {s=0; for (a=1; a<5; a+1) in s=s+2;}"
```

- [/example/multifor.ll](/example/multifor.ll)

```bash
./build/hal "def main() {s=0; t=1; for (a=1; a<5; a+1) in s=s+3; for (b=1; b<5; b+1) in t=t+2; s+t}"
```
