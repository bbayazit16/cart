# Cart

[![License: MIT](https://img.shields.io/badge/license-MIT-blue)](LICENSE)

Cart is a simple rust-like programming language with a compiler written in Rust.
The language is compiled into LLVM IR via [Inkwell](https://github.com/TheDan64/inkwell), and then to native machine
code. The object file is linked via `cc`. Unlike Rust or other low-level languages, the language is intended not to be
a systems programming language, but rather a high level language (on the level of Java) that compiles to native code.
For this reason, Cart is garbage collected and simple-to-use language.

Example Syntax: Nth Fibonacci Number

```cart
// Last value of a block automatically corresponds to the return value
func fib(n: int) -> int {
    if n <= 2 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

func main() {
    let n: int = 10;
    // Or use auto type inference
    let result = fib(n);
    // Exits program with result: No need for a semicolon if
    // it is the last expression in a block.
    result
}

```

For the formal grammar, see [grammar](grammar).

> [!NOTE]
> The language is not intended to be a replacement for any language, and should not be used seriously.

TODO:

Parsing and scanning are complete for all the following. The next steps are to generate LLVM IR for the
following:

- [x] Functions and recursion
- [x] Function calls
- [x] Blocks with last expression as return value
- [x] Return statements
- [x] Variables
- [x] If-Else
- [x] Comments
- [x] Basic Error Reporting
- [x] Basic operators
- [x] Structs
- [x] Strings
- [x] Extensions (impl blocks)
- [x] Basic Arrays/Vectors
- [ ] Loops
- [ ] More types
- [ ] Advanced Arrays/Vectors
- [ ] Enums
- [ ] Type Checking
- [ ] Generics
- [ ] Errors
- [ ] Use statements
- [ ] Pattern Matching
- [ ] More advanced error reporting
- [ ] Standard Library

## Compiler CLI Usage:

```shell
Usage: cart <COMMAND>

Commands:
  compile  Compile a file
  run      Compile and run a file
  help     Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```

## License

Cart is licensed under the MIT license. See [LICENSE](LICENSE) for more details.
