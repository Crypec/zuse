use std::fmt;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;

/// Span is used to store the src location of nodes in the compiler
#[derive(Clone)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
    pub path: PathBuf,
}

impl Span {
    pub fn new<P: Into<PathBuf>>(lo: usize, hi: usize, path: P) -> Self {
        debug_assert!(
            lo <= hi,
            "Low must be smaller or equal to high {}..{}",
            lo,
            hi
        );
        let path = path.into();
        Self { lo, hi, path }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[{:?}] :: {}..{}", self.path, self.lo, self.hi)
    }
}
