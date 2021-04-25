use konrad_ast::ast::{Lit, RangeKind};
use konrad_span::span::Span;
use parse_display::Display;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Display)]
pub enum TokenKind {
    #[display("{0}")]
    Ident(String),

    #[display("{0}")]
    Lit(Lit),

    /// Single Line Comment
    #[allow(clippy::upper_case_acronyms)]
    #[display("// {0}")]
    SLComment(String),

    /// Multi Line Comment
    #[allow(clippy::upper_case_acronyms)]
    #[display("/*\n{0}\n*/")]
    MLComment(String),

    #[display("&")]
    Ref,

    #[display("$")]
    Dollar,

    #[display("(")]
    LParen,

    #[display(")")]
    RParen,

    #[display("[")]
    LBracket,

    #[display("]")]
    RBracket,

    #[display("{{")]
    LBrace,

    #[display("}}")]
    RBrace,

    #[display("::")]
    ColCol,

    #[display(":")]
    Col,

    #[display("->")]
    ThinArrow,

    #[display("=>")]
    FatArrow,

    #[display(":=")]
    ColEq,

    #[display("=")]
    Eq,

    #[display("\n")]
    Nl,

    #[display(",")]
    Comma,

    #[display("|")]
    Sep,

    #[display(";")]
    Semi,

    #[display(".")]
    Dot,

    #[display("#")]
    Hash,

    #[display("_")]
    WildCard,

    WhiteSpace,

    /// math and boolean operator
    #[display("+")]
    Plus,

    #[display("-")]
    Minus,

    #[display("/")]
    Slash,

    #[display("*")]
    Star,

    // inclusive ranges are handled by the parser
    #[display("{0}")]
    Range(RangeKind),

    #[display("!")]
    Bang,

    #[display("&&")]
    And,
    #[display("||")]
    Or,

    #[display("^")]
    Caret,

    #[display("%")]
    Mod,

    #[display("==")]
    EqEq,

    #[display("!=")]
    NotEq,

    #[display(">")]
    Greater,

    #[display(">=")]
    GreaterEq,

    #[display("<")]
    Less,

    #[display("=<")]
    LessEq,

    #[display("{0}")]
    Keyword(Keyword),
}

/// keywords used internally by the language
#[derive(PartialEq, Debug, Eq, Clone, Display)]
pub enum Keyword {
    #[display("struct")]
    Struct,

    #[display("mut")]
    Mut,

    #[display("enum")]
    Enum,

    #[display("match")]
    Match,

    #[display("trait")]
    Trait,

    #[display("impl")]
    Impl,

    #[display("self")]
    This,

    #[display("while")]
    While,

    #[display("loop")]
    Loop,

    #[display("return")]
    Return,

    #[display("for")]
    For,

    #[display("break")]
    Break,

    #[display("continue")]
    Continue,

    #[display("if")]
    If,

    #[display("else")]
    Else,

    #[display("use")]
    Use,

    #[display("type")]
    Type,
}

impl std::str::FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "struct" => Ok(Self::Struct),
            "enum" => Ok(Self::Enum),
            "mut" => Ok(Self::Mut),
            "impl" => Ok(Self::Impl),
            "self" => Ok(Self::This),
            "while" => Ok(Self::While),
            "loop" => Ok(Self::Loop),
            "return" => Ok(Self::Return),
            "for" => Ok(Self::For),
            "if" => Ok(Self::If),
            "else" => Ok(Self::Else),
            "break" => Ok(Self::Break),
            "continue" => Ok(Self::Continue),
            "use" => Ok(Self::Use),
            "type" => Ok(Self::Type),
            "match" => Ok(Self::Match),
            "trait" => Ok(Self::Trait),
            _ => Err(()),
        }
    }
}
