use konrad_err::diagnostic::*;
use konrad_lexer::{lexer::*, token::*};
use konrad_span::span::*;
use pretty_assertions::assert_eq;

// This is just for testing, this file does not exist
const TEST_FILE_PATH: &str = "TEST_FILE_PLACEHOLDER.zs";

fn span(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Span {
    let start_lc = LineColumn::new(start_line, start_col);
    let end_lc = LineColumn::new(end_line, end_col);
    Span::new(start_lc, end_lc, TEST_FILE_PATH)
}

macro_rules! assert_spans_eq {
    ($src:expr, $($expected:expr),+ $(,)?) => {
        let src: &str = $src;
        let expected: Vec<Span> = vec![$($expected),+];

        let tokens = get_filtered_tokens(src);

        let actual: Vec<_> = tokens.iter().map(|t| t.span.clone()).collect();

        assert_eq!(actual, expected, "tokens: {:#?}", tokens);
    };
}

#[test]
fn test_spans_enum_decl() {
    assert_spans_eq!(
        r#"Weekday :: enum {
            Monday,
            Tuesday,
            Wednesday,
        }"#,
        span(0, 0, 0, 6),
        span(0, 8, 0, 9),
        span(0, 11, 0, 14),
        span(0, 16, 0, 16),
        span(1, 12, 1, 17),
        span(1, 18, 1, 18),
        span(2, 12, 2, 18),
        span(2, 19, 2, 19),
        span(3, 12, 3, 20),
        span(3, 21, 3, 21),
        span(4, 8, 4, 8),
    );
}

#[test]
fn test_span_person_struct_decl() {
    assert_spans_eq!(
        r#"Person :: struct {
            age: s64,
            name: String,
            relatives: Vec<String>,
        }"#,
        span(0, 0, 0, 5),
        span(0, 7, 0, 8),
        span(0, 10, 0, 15),
        span(0, 17, 0, 17),
        span(1, 12, 1, 14),
        span(1, 15, 1, 15),
        span(1, 17, 1, 19),
        span(1, 20, 1, 20),
        span(2, 12, 2, 15),
        span(2, 16, 2, 16),
        span(2, 18, 2, 23),
        span(2, 24, 2, 24),
        span(3, 12, 3, 20),
        span(3, 21, 3, 21),
        span(3, 23, 3, 25),
        span(3, 26, 3, 26),
        span(3, 27, 3, 32),
        span(3, 33, 3, 33),
        span(3, 34, 3, 34),
        span(4, 8, 4, 8),
    );
}

#[test]
fn test_span_foo_bar_var_def() {
    assert_spans_eq!(
        "mut foo := bar",
        span(0, 0, 0, 2),
        span(0, 4, 0, 6),
        span(0, 8, 0, 9),
        span(0, 11, 0, 13),
    );
}

#[test]
fn test_span_string_lit_assign() {
    assert_spans_eq!(
        r#" foo := "Hello World""#,
        span(0, 1, 0, 3),
        span(0, 5, 0, 6),
        span(0, 8, 0, 20),
    );
}

#[test]
fn test_span_array_lit() {
    assert_spans_eq!(
        "[1, 20, 3, 4, 5]",
        span(0, 0, 0, 0),
        span(0, 1, 0, 1),
        span(0, 2, 0, 2),
        span(0, 4, 0, 5),
        span(0, 6, 0, 6),
        span(0, 8, 0, 8),
        span(0, 9, 0, 9),
        span(0, 11, 0, 11),
        span(0, 12, 0, 12),
        span(0, 14, 0, 14),
        span(0, 15, 0, 15),
    );
}

#[test]
fn test_span_dec_num() {
    assert_spans_eq!("0123456789", span(0, 0, 0, 9));
}

#[test]
fn test_span_hex_num() {
    assert_spans_eq!("0x0123456789ABCDEF", span(0, 0, 0, 17));
}

#[test]
fn test_span_range_decl() {
    assert_spans_eq!(
        "ArenaIndex :: 0..10;",
        span(0, 0, 0, 9),
        span(0, 11, 0, 12),
        span(0, 14, 0, 14),
        span(0, 15, 0, 16),
        span(0, 17, 0, 18),
        span(0, 19, 0, 19),
    );
}

#[test]
fn test_span_punctuation_tokens() {
    assert_spans_eq!(
        ":: : {} () [] | || , ; . .. = _ := ->",
        span(0, 0, 0, 1),
        span(0, 3, 0, 3),
        span(0, 5, 0, 5),
        span(0, 6, 0, 6),
        span(0, 8, 0, 8),
        span(0, 9, 0, 9),
        span(0, 11, 0, 11),
        span(0, 12, 0, 12),
        span(0, 14, 0, 14),
        span(0, 16, 0, 17),
        span(0, 19, 0, 19),
        span(0, 21, 0, 21),
        span(0, 23, 0, 23),
        span(0, 25, 0, 26),
        span(0, 28, 0, 28),
        span(0, 30, 0, 30),
        span(0, 32, 0, 33),
        span(0, 35, 0, 36),
    );
}

#[test]
fn test_span_idents() {
    assert_spans_eq!(
        "foo bar_10 test_1_1 bar__f00 D3ADB33F",
        span(0, 0, 0, 2),
        span(0, 4, 0, 9),
        span(0, 11, 0, 18),
        span(0, 20, 0, 27),
        span(0, 29, 0, 36),
    );
}

#[test]
fn test_span_poly_array_ty() {
    assert_spans_eq!(
        "[$T]",
        span(0, 0, 0, 0),
        span(0, 1, 0, 1),
        span(0, 2, 0, 2),
        span(0, 3, 0, 3),
    );
}

#[test]
fn test_span_b10_num() {
    assert_spans_eq!("100_200_300", span(0, 0, 0, 10));
}

#[test]
fn test_span_keywords() {
    assert_spans_eq!(
        "struct enum type self while return for if else break continue loop",
        span(0, 0, 0, 5),
        span(0, 7, 0, 10),
        span(0, 12, 0, 15),
        span(0, 17, 0, 20),
        span(0, 22, 0, 26),
        span(0, 28, 0, 33),
        span(0, 35, 0, 37),
        span(0, 39, 0, 40),
        span(0, 42, 0, 45),
        span(0, 47, 0, 51),
        span(0, 53, 0, 60),
        span(0, 62, 0, 65),
    );
}

#[test]
fn test_span_range() {
    assert_spans_eq!(
        ".. ..= .. = .",
        span(0, 0, 0, 1),
        span(0, 3, 0, 5),
        span(0, 7, 0, 8),
        span(0, 10, 0, 10),
        span(0, 12, 0, 12),
    );
}

#[test]
fn test_span_bin_literal() {
    assert_spans_eq!(
        "0b10_0101_0111, 0b1111, 0b0000",
        span(0, 0, 0, 13),
        span(0, 14, 0, 14),
        span(0, 16, 0, 21),
        span(0, 22, 0, 22),
        span(0, 24, 0, 29),
    );
}

#[test]
fn test_span_oct_literal() {
    assert_spans_eq!("0o1256235", span(0, 0, 0, 8));
}

#[test]
fn test_span_if_stmt() {
    assert_spans_eq!(
        r#"
        if foo == bar {
            return 4 + 2;
        } else if foo != bar {
            return 4 / 2;
        } else {
            return ANSWER_TO_LIFE;
        }
        "#,
        span(1, 8, 1, 9),
        span(1, 11, 1, 13),
        span(1, 15, 1, 16),
        span(1, 18, 1, 20),
        span(1, 22, 1, 22),
        span(2, 12, 2, 17),
        span(2, 19, 2, 19),
        span(2, 21, 2, 21),
        span(2, 23, 2, 23),
        span(2, 24, 2, 24),
        span(3, 8, 3, 8),
        span(3, 10, 3, 13),
        span(3, 15, 3, 16),
        span(3, 18, 3, 20),
        span(3, 22, 3, 23),
        span(3, 25, 3, 27),
        span(3, 29, 3, 29),
        span(4, 12, 4, 17),
        span(4, 19, 4, 19),
        span(4, 21, 4, 21),
        span(4, 23, 4, 23),
        span(4, 24, 4, 24),
        span(5, 8, 5, 8),
        span(5, 10, 5, 13),
        span(5, 15, 5, 15),
        span(6, 12, 6, 17),
        span(6, 19, 6, 32),
        span(6, 33, 6, 33),
        span(7, 8, 7, 8),
    );
}

#[test]
fn test_span_match() {
    assert_spans_eq!(
        r#"
        match foo {
            0 | 1 => 42,
            _ => 42,
        }
        "#,
        span(1, 8, 1, 12),
        span(1, 14, 1, 16),
        span(1, 18, 1, 18),
        span(2, 12, 2, 12),
        span(2, 14, 2, 14),
        span(2, 16, 2, 16),
        span(2, 18, 2, 19),
        span(2, 21, 2, 22),
        span(2, 23, 2, 23),
        span(3, 12, 3, 12),
        span(3, 14, 3, 15),
        span(3, 17, 3, 18),
        span(3, 19, 3, 19),
        span(4, 8, 4, 8),
    );
}

#[test]
fn test_span_trait_decl() {
    assert_spans_eq!(
        r#"
        trait isEven {
            is_even :: (&self) -> bool;
        }
        "#,
        span(1, 8, 1, 12),
        span(1, 14, 1, 19),
        span(1, 21, 1, 21),
        span(2, 12, 2, 18),
        span(2, 20, 2, 21),
        span(2, 23, 2, 23),
        span(2, 24, 2, 24),
        span(2, 25, 2, 28),
        span(2, 29, 2, 29),
        span(2, 31, 2, 32),
        span(2, 34, 2, 37),
        span(2, 38, 2, 38),
        span(3, 8, 3, 8),
    );
}

fn get_filtered_tokens<S: Into<String>>(src: S) -> Vec<Token> {
    #[derive(Debug)]
    struct ErrDump<T> {
        tokens: Vec<T>,
        errs:   Vec<Diagnostic>,
    }

    let (tokens, errs): (Vec<Result<Token, Diagnostic>>, Vec<Result<Token, Diagnostic>>) =
        Lexer::new(src, TEST_FILE_PATH).partition(Result::is_ok);
    let tokens: Vec<_> = tokens
        .into_iter()
        .map(Result::unwrap)
        .filter(|t| !t.kind.is_whitespace())
        .collect();
    let errs: Vec<_> = errs.into_iter().map(Result::unwrap_err).collect();
    if !errs.is_empty() {
        assert!(
            errs.is_empty(),
            "Encountered errors during test_spaning: {:#?}",
            ErrDump { tokens, errs }
        );
    }
    tokens
}
