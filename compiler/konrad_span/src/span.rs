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

    fn merge(&self, other: &Self) -> Self {
        debug_assert_eq!(self.path, other.path, "Paths must be equal");

        let start_line = std::cmp::min(self.start.line, self.end.line);
        let end_line = std::cmp::max(self.start.line, self.end.line);

        let start_col = std::cmp::min(self.start.col, self.end.col);
        let end_col = std::cmp::max(self.start.col, self.end.col);

        Self {
            start: LineColumn::new(start_line, start_col),
            end: LineColumn::new(end_line, end_col),
            path: self.path.clone(),
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
