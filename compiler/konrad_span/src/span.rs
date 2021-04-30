use std::cmp::*;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
/// It is treated like an inclusive range!
#[derive(Clone, Eq, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
    pub path: PathBuf,
}

impl Span {
    pub fn new<P: Into<PathBuf>>(lhs: LineColumn, rhs: LineColumn, path: P) -> Self {
        let start = std::cmp::min(lhs, rhs);
        let end = std::cmp::max(lhs, rhs);
        let path = path.into();
        Self { start, end, path }
    }

    fn merge(&self, other: &Self) -> Self {
        debug_assert_eq!(
            self.path, other.path,
            "Paths must be equal: {:?} != {:?}",
            self.path, other.path,
        );

        let start = *std::cmp::min(&self.start, &other.start);
        let end = *std::cmp::max(&self.end, &other.end);

        Self {
            start,
            end,
            path: self.path.clone(),
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // NOTE(Simon): We use zero indexing for the line numbers so we need to add 1 to
        // NOTE(Simon): show the correct line number!
        if self.end.line == self.start.line {
            write!(
                f,
                "[{:?}] ----> [line: {}] :: [col: {}..={}]",
                self.path,
                self.start.line + 1,
                self.start.col,
                self.end.col
            )
        } else {
            write!(
                f,
                "[{:?}] ----> [line: {}..={}] :: [col: {}..={}]",
                self.path,
                self.start.line + 1,
                self.end.line + 1,
                self.start.col,
                self.end.col
            )
        }
    }
}

impl Ord for LineColumn {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.line != other.line {
            self.line.cmp(&other.line)
        } else {
            self.col.cmp(&other.col)
        }
    }
}

impl PartialOrd for LineColumn {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const DUMMY_PATH: &str = "TEST_PATH_DOES_NOT_EXIST.zs";

    #[test]
    fn test_span_new() {
        let actual = {
            let lhs_lc = LineColumn::new(25, 42);
            let rhs_lc = LineColumn::new(20, 10);
            Span::new(lhs_lc, rhs_lc, DUMMY_PATH)
        };
        let expected = Span {
            start: LineColumn { line: 20, col: 10 },
            end: LineColumn { line: 25, col: 42 },
            path: DUMMY_PATH.into(),
        };
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_span_merge() {
        let lhs = {
            let lhs_lc = LineColumn::new(25, 42);
            let rhs_lc = LineColumn::new(20, 10);
            Span::new(lhs_lc, rhs_lc, DUMMY_PATH)
        };
        let rhs = {
            let lhs_lc = LineColumn::new(10, 0);
            let rhs_lc = LineColumn::new(19, 0);
            Span::new(lhs_lc, rhs_lc, DUMMY_PATH)
        };
        let actual = rhs.merge(&lhs);
        let expected = Span {
            start: LineColumn { line: 10, col: 0 },
            end: LineColumn { line: 25, col: 42 },
            path: DUMMY_PATH.into(),
        };
        assert_eq!(actual, expected);
    }
}
