use float_cmp::*;
use konrad_span::span::Span;
use parse_display::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Display)]
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

    #[display("{0}")]
    Range(RangeKind),

    #[display("!")]
    Bang,

    #[display("&&")]
    And,

    #[display("||")]
    Or,

    #[display("^^")]
    Xor,

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

    #[display(">>")]
    LShift,

    #[display("<<")]
    RShift,

    #[display("{0}")]
    Keyword(Keyword),
}

/// keywords used internally by the language
#[derive(PartialEq, Eq, Debug, Clone, Display)]
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

    #[display("type")]
    Type,
}

#[derive(Debug, Clone, Display)]
pub enum Lit {
    // NOTE(Simon): we store all numbers as positives. The sign get's encoded into the AST
    #[display("{}")]
    Num(u128),
    #[display("{}")]
    Float(f64),
    #[display("{}")]
    Text(String),
}

impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => a == b,
            (Self::Text(a), Self::Text(b)) => a == b,
            // NOTE(Simon): I don't know if the parameters for approx_eq are correct
            // NOTE(Simon): I just pulled them directly from their example
            // NOTE(Simon): If we ever run into an issue with float comparisons we might have to
            // NOTE(Simon): look at this again!
            //              - Simon Kunz 05.05.2021
            (Self::Float(a), Self::Float(b)) => a.approx_eq(*b, (0.0, 2)),
            _ => false,
        }
    }
}

impl Eq for Lit {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
pub enum RangeKind {
    #[display("..=")]
    Inclusive,
    #[display("..")]
    Exclusive,
}

impl TokenKind {
    pub const fn is_whitespace(&self) -> bool {
        matches!(
            self,
            Self::WhiteSpace | Self::Nl | Self::SLComment(_) | Self::MLComment(_)
        )
    }
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
            "type" => Ok(Self::Type),
            "match" => Ok(Self::Match),
            "trait" => Ok(Self::Trait),
            _ => Err(()),
        }
    }
}
