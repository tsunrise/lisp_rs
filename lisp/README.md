### 🚧 With in progress

# LISP-Prototype

This is a prototype of a Lisp-like language interpreter/compiler. Most parser code and interpreter code are adapted from
Berkeley CS164 Fall 2021 class compiler (https://github.com/berkeley-cs164-2021/164-class-compiler).

## Current Progress

#### 📝 Parser / Lexer

Done! Check [lex.rs](./src/lex.rs), [s_exp.rs](./src/s_exp.rs), [ast.rs](./src/ast.rs).

#### 📝 Interpreter

- [x] Primitives
- [x] Function Call
- [ ] Closure

#### 📝 Compiler

Unlike CS164 compiler, this crate plans to use LLVM backend. The compiler is in very, very early stage (and does not
work yet).

- [x] Primitives (partly finished)
- [ ] Control Flow
- [x] Function Call
- [ ] Function Inlining
- [ ] ...

Fow now, the compiler can only emit LLVM IR code, and does not generate native code yet.

## Build Guide

The library compiles on the `stable` toolchain of the Rust compiler. To install the latest version of Rust, first
install `rustup` by following the instructions [here](https://rustup.rs/), or via your platform's package manager.
Once `rustup` is installed, install the Rust toolchain by invoking:

```bash
rustup install stable
```

After that, use `cargo`, the standard Rust build tool, to build the library:

```bash
git clone https://github.com/tsunrise/lisp_rs.git
cargo build --release
```

To use compiler feature, you need to have LLVM-13 installed on your system. LLVM can be downloaded
in https://releases.llvm.org/download.html#13.0.0. Check out more details on https://crates.io/crates/llvm-sys.

This library comes with unit tests. Run the tests with:

```bash
cargo test
```


