use konrad_ast::ast::Lit;
use konrad_err::diagnostic::*;
use konrad_lexer::lexer::Lexer;
use konrad_lexer::token::*;
use pretty_assertions::assert_eq;

// This is just for testing, this file does not exist
const TEST_FILE_PATH: &str = "TEST_FILE_PLACEHOLDER.zs";

macro_rules! assert_tokens_eq {
    ($src:expr, $($expected:expr),+ $(,)?) => {
        let src: &str = $src;
        let expected: Vec<TokenKind> = vec![$($expected),+];
        let actual = get_filtered_tokens(src);
        assert_eq!(actual, expected);
    };
}

#[test]
fn lex_weekday_enum_decl() {
    assert_tokens_eq!(
        r#"Weekday :: enum {
            Monday,
            Tuesday,
            Wednesday,
        }"#,
        TokenKind::Ident("Weekday".into()),
        TokenKind::ColCol,
        TokenKind::Keyword(Keyword::Enum),
        TokenKind::LBrace,
        TokenKind::Ident("Monday".into()),
        TokenKind::Comma,
        TokenKind::Ident("Tuesday".into()),
        TokenKind::Comma,
        TokenKind::Ident("Wednesday".into()),
        TokenKind::Comma,
        TokenKind::RBrace,
    );
}

#[test]
fn lex_person_struct_decl() {
    assert_tokens_eq!(
        r#"Person :: struct {
            age: s64,
            name: String,
            relatives: Vec<String>,
        }"#,
        TokenKind::Ident("Person".to_string()),
        TokenKind::ColCol,
        TokenKind::Keyword(Keyword::Struct),
        TokenKind::LBrace,
        TokenKind::Ident("age".to_string()),
        TokenKind::Col,
        TokenKind::Ident("s64".to_string()),
        TokenKind::Comma,
        TokenKind::Ident("name".to_string()),
        TokenKind::Col,
        TokenKind::Ident("String".to_string()),
        TokenKind::Comma,
        TokenKind::Ident("relatives".to_string()),
        TokenKind::Col,
        TokenKind::Ident("Vec".to_string()),
        TokenKind::Greater,
        TokenKind::Ident("String".to_string()),
        TokenKind::Less,
        TokenKind::Comma,
        TokenKind::RBrace,
    );
}

#[test]
fn lex_foo_bar_var_def() {
    assert_tokens_eq!(
        "mut foo := bar",
        TokenKind::Keyword(Keyword::Mut),
        TokenKind::Ident("foo".into()),
        TokenKind::ColEq,
        TokenKind::Ident("bar".into()),
    );
}

#[test]
fn lex_string_lit_assign() {
    assert_tokens_eq!(
        r#" foo := "Hello World""#,
        TokenKind::Ident("foo".into()),
        TokenKind::ColEq,
        TokenKind::Lit(Lit::Text("Hello World".into())),
    );
}

#[test]
fn lex_array_lit() {
    assert_tokens_eq!(
        "[1, 2, 3, 4, 5]",
        TokenKind::LBracket,
        TokenKind::Lit(Lit::Num(1)),
        TokenKind::Comma,
        TokenKind::Lit(Lit::Num(2)),
        TokenKind::Comma,
        TokenKind::Lit(Lit::Num(3)),
        TokenKind::Comma,
        TokenKind::Lit(Lit::Num(4)),
        TokenKind::Comma,
        TokenKind::Lit(Lit::Num(5)),
        TokenKind::RBracket,
    );
}

#[test]
fn lex_dec_num() {
    assert_tokens_eq!("0123456789", TokenKind::Lit(Lit::Num(0123456789)));
}

#[test]
fn lex_hex_num() {
    assert_tokens_eq!(
        "0x0123456789ABCDEF",
        TokenKind::Lit(Lit::Num(0x0123456789ABCDEF))
    );
}

#[test]
fn lex_range_decl() {
    assert_tokens_eq!(
        "ArenaIndex :: 0..10;",
        TokenKind::Ident("ArenaIndex".into()),
        TokenKind::ColCol,
        TokenKind::Lit(Lit::Num(0)),
        TokenKind::Range(Range::Exclusive),
        TokenKind::Lit(Lit::Num(10)),
        TokenKind::Semi,
    );
}

#[test]
fn lex_punctuation_tokens() {
    assert_tokens_eq!(
        ":: : {} () [] | || , ; . .. = _ := ->",
        TokenKind::ColCol,
        TokenKind::Col,
        TokenKind::LBrace,
        TokenKind::RBrace,
        TokenKind::LParen,
        TokenKind::RParen,
        TokenKind::LBracket,
        TokenKind::RBracket,
        TokenKind::Sep,
        TokenKind::Or,
        TokenKind::Comma,
        TokenKind::Semi,
        TokenKind::Dot,
        TokenKind::Range(Range::Exclusive),
        TokenKind::Eq,
        TokenKind::WildCard,
        TokenKind::ColEq,
        TokenKind::ThinArrow,
    );
}

#[test]
fn lex_idents() {
    assert_tokens_eq!(
        "foo bar_10 test_1_1 bar__f00 D3ADB33F",
        TokenKind::Ident("foo".into()),
        TokenKind::Ident("bar_10".into()),
        TokenKind::Ident("test_1_1".into()),
        TokenKind::Ident("bar__f00".into()),
        TokenKind::Ident("D3ADB33F".into()),
    );
}

#[test]
fn lex_poly_array_ty() {
    assert_tokens_eq!(
        "[$T]",
        TokenKind::LBracket,
        TokenKind::Dollar,
        TokenKind::Ident("T".into()),
        TokenKind::RBracket,
    );
}

#[test]
fn lex_b10_num() {
    assert_tokens_eq!("100_200_300", TokenKind::Lit(Lit::Num(100_200_300)));
}

#[test]
fn lex_keywords() {
    assert_tokens_eq!(
        "struct enum type self while return for if else break continue loop",
        TokenKind::Keyword(Keyword::Struct),
        TokenKind::Keyword(Keyword::Enum),
        TokenKind::Keyword(Keyword::Type),
        TokenKind::Keyword(Keyword::This),
        TokenKind::Keyword(Keyword::While),
        TokenKind::Keyword(Keyword::Return),
        TokenKind::Keyword(Keyword::For),
        TokenKind::Keyword(Keyword::If),
        TokenKind::Keyword(Keyword::Else),
        TokenKind::Keyword(Keyword::Break),
        TokenKind::Keyword(Keyword::Continue),
        TokenKind::Keyword(Keyword::Loop),
    );
}

#[test]
fn lex_range() {
    assert_tokens_eq!(
        ".. ..= .. = .",
        TokenKind::Range(Range::Exclusive),
        TokenKind::Range(Range::Inclusive),
        TokenKind::Range(Range::Exclusive),
        TokenKind::Eq,
        TokenKind::Dot,
    );
}

#[test]
fn lex_bin_literal() {
    assert_tokens_eq!(
        "0b10_0101_0111, 0b1111, 0b0000",
        TokenKind::Lit(Lit::Num(0b1001010111)),
        TokenKind::Comma,
        TokenKind::Lit(Lit::Num(0b1111)),
        TokenKind::Comma,
        TokenKind::Lit(Lit::Num(0)),
    );
}

#[test]
fn lex_oct_literal() {
    assert_tokens_eq!("0o1256235", TokenKind::Lit(Lit::Num(0o1256235)));
}

#[test]
fn lex_if_stmt() {
    assert_tokens_eq!(
        r#"
        if foo == bar {
            return 4 + 2;
        } else if foo != bar {
            return 4 / 2;
        } else {
            return ANSWER_TO_LIFE;
        }
    "#,
        TokenKind::Keyword(Keyword::If),
        TokenKind::Ident("foo".into()),
        TokenKind::EqEq,
        TokenKind::Ident("bar".into()),
        TokenKind::LBrace,
        TokenKind::Keyword(Keyword::Return),
        TokenKind::Lit(Lit::Num(4)),
        TokenKind::Plus,
        TokenKind::Lit(Lit::Num(2)),
        TokenKind::Semi,
        TokenKind::RBrace,
        TokenKind::Keyword(Keyword::Else),
        TokenKind::Keyword(Keyword::If),
        TokenKind::Ident("foo".into()),
        TokenKind::NotEq,
        TokenKind::Ident("bar".into()),
        TokenKind::LBrace,
        TokenKind::Keyword(Keyword::Return),
        TokenKind::Lit(Lit::Num(4)),
        TokenKind::Slash,
        TokenKind::Lit(Lit::Num(2)),
        TokenKind::Semi,
        TokenKind::RBrace,
        TokenKind::Keyword(Keyword::Else),
        TokenKind::LBrace,
        TokenKind::Keyword(Keyword::Return),
        TokenKind::Ident("ANSWER_TO_LIFE".into()),
        TokenKind::Semi,
        TokenKind::RBrace,
    );
}

#[test]
fn lex_match() {
    assert_tokens_eq!(
        r#"
        match foo {
            0 | 1 => 42,
            _ => 42,
        }
        "#,
        TokenKind::Keyword(Keyword::Match),
        TokenKind::Ident("foo".into()),
        TokenKind::LBrace,
        TokenKind::Lit(Lit::Num(0)),
        TokenKind::Sep,
        TokenKind::Lit(Lit::Num(1)),
        TokenKind::FatArrow,
        TokenKind::Lit(Lit::Num(42)),
        TokenKind::Comma,
        TokenKind::WildCard,
        TokenKind::FatArrow,
        TokenKind::Lit(Lit::Num(42)),
        TokenKind::Comma,
        TokenKind::RBrace,
    );
}

#[test]
fn lex_fn_definition() {
    assert_tokens_eq!(
        r#"
        trait isEven {
            is_even :: (&self) -> bool;
        }
        "#,
        TokenKind::Keyword(Keyword::Trait),
        TokenKind::Ident("isEven".into()),
        TokenKind::LBrace,
        TokenKind::Ident("is_even".into()),
        TokenKind::ColCol,
        TokenKind::LParen,
        TokenKind::Ref,
        TokenKind::Keyword(Keyword::This),
        TokenKind::RParen,
        TokenKind::ThinArrow,
        TokenKind::Ident("bool".into()),
        TokenKind::Semi,
        TokenKind::RBrace,
    );
}

fn get_filtered_tokens<S: Into<String>>(src: S) -> Vec<TokenKind> {
    fn should_be_included(t: &TokenKind) -> bool {
        !(matches!(t, TokenKind::Nl | TokenKind::WhiteSpace))
    }

    let (tokens, errs): (
        Vec<Result<Token, Diagnostic>>,
        Vec<Result<Token, Diagnostic>>,
    ) = Lexer::new(src, TEST_FILE_PATH).partition(Result::is_ok);
    let tokens: Vec<_> = tokens
        .into_iter()
        .map(Result::unwrap)
        .map(|t| t.kind)
        .filter(should_be_included)
        .collect();
    let errs: Vec<_> = errs.into_iter().map(Result::unwrap_err).collect();
    if !errs.is_empty() {
        #[derive(Debug)]
        struct LexError {
            tokens: Vec<TokenKind>,
            errs: Vec<Diagnostic>,
        }
        assert!(
            errs.is_empty(),
            "Encountered errors during lexing: {:#?}",
            LexError { tokens, errs }
        );
    }
    tokens
}
