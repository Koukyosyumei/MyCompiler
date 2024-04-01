# MyCompilier

This project implements a compiler that translates code from a simple language into LLVM IR. This project is inspiered by [Kaleidoscope](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html).

What it does:

- Takes source code as input and breaks it down into its building blocks (identifiers, numbers, expressions) using a lexer and parser.
- Generates LLVM IR that represents the functionality of the source code. This code can then be compiled and run by an LLVM toolchain.

What it can do:

- Handles basic expressions involving arithmetic operations (addition, multiplication, comparison).
- Supports function definitions with arguments and function calls.
- Generates code for conditional statements (if statements)

Examples

```
./main "def main() if 4<3 then 12 else 13;" | lli; echo $?
```
