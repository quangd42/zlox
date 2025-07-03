# zlox

A bytecode virtual machine implementation of the Lox language interpreter, written in [Zig](https://ziglang.org/).

This is the implementation for second half of the book [Crafting Interpreters](https://craftinginterpreters.com/). For the first half, a tree-walk interpreter implementation written in Go [can be found here.](https://github.com/quangd42/golox)

## Usage

[Zig 0.14](https://ziglang.org/learn/getting-started/) is required to build and run this project.

To execute a lox script:

```sh
just run test/field/get_and_set_method.lox
```

Or omit the file path to kick off the repl.
