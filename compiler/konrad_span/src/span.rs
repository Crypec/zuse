use std::fmt;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;

#[derive(Clone, PartialEq, Eq)]
pub struct LineColumn {
    pub line: usize,
    pub col: usize,
}

impl LineColumn {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

/// Span is used to store the src location of nodes in the compiler
#[derive(Clone, Eq, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
    pub path: PathBuf,
}

impl Span {
    pub fn new<P: Into<PathBuf>>(start: LineColumn, end: LineColumn, path: P) -> Self {
        debug_assert!(
            start.line <= end.line,
            "start must be smaller or equal to end start: {} :: end: {}",
            start.line,
            end.line,
        );
        let path = path.into();
        Self { start, end, path }
    }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // NOTE(Simon): We use zero indexing for the line numbers so we need to add one
        // NOTE(Simon): show the correct line number!
        write!(
            f,
            "[{:?}] ----> [{} :: {}]..[{} :: {}]",
            self.path,
            self.start.line + 1,
            self.start.col,
            self.end.line + 1,
            self.end.col
        )
    }
}
