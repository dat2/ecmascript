# ecmascript ![Travis](https://img.shields.io/travis/dat2/ecmascript.svg) ![Codecov](https://img.shields.io/codecov/c/github/dat2/ecmascript.svg)

This is a rust crate to help you with ECMAScript 2017 v8.0. It provides a
parser and an AST (abstract syntax tree) implementation. We also provide
some macros to construct the AST so you can do interesting things like
optimization!

# Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
ecmascript = "0.1"
```

Then put this in your crate root:

```rust
extern crate ecmascript;
```

# Example

This example reads a file, parses it, and then prints out a minified version.

The file reading is taken from the [`std::fs::File` docs](https://doc.rust-lang.org/std/fs/struct.File.html)

```rust
extern crate ecmascript;

use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
	// read foo.js
    let mut file = File::open("foo.js")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

	// parse it
    let ast = ecmascript::parse(&contents).unwrap();
    let minifed = ecmascript::minify(&ast);
    println!("{}", minified);
}
```

# API Reference

Docs are hosted on [docs.rs](https://docs.rs/ecmascript/).

# About

`ecmascript` is used to parse a JavaScript module, and perform some operations
on it. For example, concatenating modules together, uglifying the variable names,
pretty printing uglified code, etc.

# Features

* _ECMAScript 2017 v8.0 support_
  * We are actively developing this library to be up to date!
* _JSX Extended Support_
  * JSX is meant to be an additive extension to the language
* _AST Pretty Printer_
  * This supports minification options, such as 0 whitespace
* _AST rewrite rules_
  * Eg. Constant folding, translating to older versions of the language, etc.

# Testing

To test everything, just run this command:

```
cargo test
```

Or to run a single test,

```
cargo test --test <test_name>
```

# Linting

To lint your code, use [clippy](https://github.com/rust-lang-nursery/rust-clippy). Its as easy as
running once you have it installed!

```
cargo clippy
```

# TO DO

* [ ] Parser Integration Testing
  * eg. reading files, asserting that the parse is succesful
* [ ] Re-write the grammar from the spec into a readable document
* [ ] Build macros to help match AST / build AST trees

# References

* http://www.ecma-international.org/ecma-262/8.0/

# License

MIT © Nick Dujay
