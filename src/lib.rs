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
