#![feature(or_patterns)]
#![feature(trace_macros)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![warn(clippy::nursery)]
#![warn(clippy::perf)]

pub mod parser;
pub use parser::*;
