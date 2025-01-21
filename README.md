# Lox

This is an implementation of the Lox programming language. It follows the book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

## Usage

```bash
cargo run --release -p lox_interpreter -- <program>
```

## Features

- [x] Basic Expression Evaluation
  - [x] Arithmetic operations
  - [x] Boolean operations
  - [x] Comparison operators
- [x] Variables and State
  - [x] Variable declaration and assignment
  - [x] Block scoping
  - [x] Static scoping
- [x] Control Flow
  - [x] If statements
  - [x] While loops
  - [x] For loops
  - [x] Block statements
- [x] Functions
  - [x] Function declarations
  - [x] Return statements
  - [x] Function calls
- [x] Closures
  - [x] First-class functions
  - [x] Lexical scoping
  - [x] Closure creation and usage
- [ ] Classes
  - [ ] Class declarations
  - [ ] Method declarations
  - [ ] Inheritance
  - [ ] This keyword
- [ ] Bytecode Compiler
- [ ] Bytecode VM
