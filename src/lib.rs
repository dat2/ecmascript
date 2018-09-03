#![deny(missing_docs)]
#![recursion_limit = "256"]

//! # ecmascript
//! `ecmascript` is a crate that helps you parse the ECMAScript 2017 v8.0 language.
//! It also provides some useful macros to help you construct the AST
//! if you want to perform some operations on it.

#[macro_use]
extern crate combine;
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate unicode_xid;
#[macro_use]
extern crate serde_derive;

extern crate serde;

#[macro_use]
mod macros;
pub mod ast;
mod parser;
#[cfg(test)]
mod parser_unit_test;

pub use parser::parse;
