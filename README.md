# zlox

A bytecode virtual machine implementation of the Lox language interpreter, written in [Zig](https://ziglang.org/).

This is the implementation for second half of the book [Crafting Interpreters](https://craftinginterpreters.com/): a compiler and stack-based VM with manual memory management and garbage collection.
A tree-walk interpreter for the first half (written in Go) is [available here.](https://github.com/quangd42/golox)

## Demo
The following demo showcases:
- Interactive REPL usage
- Functions and closures
- Classes and method dispatch

[Video](#)

## Usage

[Zig 0.14.1](https://ziglang.org/learn/getting-started/) is required to build and run this project.

```sh
# Build (release mode, NaN-boxing enabled)
# The binary is stored in zig-out/bin
zig build -Doptimize=ReleaseSafe -Dnan-boxing

# Run the full test suite
zig build test --summary new

# Test with specific Lox scripts
zig build test -- test/assignment/associativity.lox test/variable/collide_with_parameter.lox

# Run a Lox script
./zig-out/bin/zlox test/field/get_and_set_method.lox

# Start the REPL
./zig-out/bin/zlox
```
