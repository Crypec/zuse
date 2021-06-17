use crate::compiler::*;

mod compiler;
mod workspace;

fn main() {
    let mut c = Compiler::new();
    c.run();
}
