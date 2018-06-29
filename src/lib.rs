#![deny(missing_docs)]
//! # ecmascript
//! `ecmascript` is a crate that helps you parse the ECMAScript 2017 v8.0 language.
//! It also provides some useful macros to help you construct the AST
//! if you want to perform some operations on it.

extern crate combine;
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate unicode_xid;

#[macro_use]
mod macros;
pub mod ast;
pub mod parser;

pub use parser::parse;
