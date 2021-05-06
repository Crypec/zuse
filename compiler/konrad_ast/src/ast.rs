use derivative::*;
use std::convert::TryFrom;

use konrad_err::diagnostic::*;
use konrad_lexer::token::*;
use konrad_span::span::Span;

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum ExprKind {
    /// normal binary expression, only used for numeric expressions
    /// example: a      +     42
    ///          ^-rhs  ^-op  ^-lhs
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: BinOp,
    },

    /// one sided expression
    /// example: -    3
    ///          ^-op ^-rhs
    Unary {
        rhs: Box<Expr>,
        op: UnaryOp,
    },

    /// struct literals are used to initialize objects with values
    /// example: Person {name: "Torben"}
    ///          ^-name  ^^^^^^^^^^^^^^- member with name and init expr
    Struct {
        name: Ident,
        members: Vec<(String, Expr)>,
    },

    /// a tuple expression is just a collection of other expressions
    /// example: (20,    20)
    ///           ^-expr ^-expr
    #[derivative(Debug = "transparent")]
    Tup(Vec<Expr>),

    /// variable reference, possibly containing `::` to refer to types in other moduels
    /// example: foo::bar
    ///          ^^^-segment
    #[derivative(Debug = "transparent")]
    Path(Path),

    Var(Ident),

    /// used to represent all sorts of index expressions
    /// example: foo[     expr     ]
    ///          ^-callee ^index
    Index {
        callee: Box<Expr>,
        index: Box<Expr>,
    },

    /// array literals are used to initialize arrays with values
    /// example: [1, 2, 3, 4, 5]
    ///           ^-create new array with values from 1 to including 5

    #[derivative(Debug = "transparent")]
    Array(Vec<Expr>),

    /// a range pattern
    /// example: 0   ..   10
    ///           ^-start ^-end
    Range {
        lo: Box<Expr>,
        hi: Box<Expr>,
        kind: RangeKind,
    },

    /// a raw literal is the smallest possible expression
    /// example: 42
    ///          ^-num literal
    /// example: "foo"
    ///          ^-string/text literal
    #[derivative(Debug = "transparent")]
    Lit(Lit),

    /// access of a named struct field like a.b
    /// example: a  .    b
    ///          ^-callee ^ field
    Field(Box<Expr>, Ident),

    /// refers to a object instance and can be used to refer to that instance and it's member fields e.g. (selbst.foo)
    /// if a function contains self as an argument in it's signature it automatically becomes an associated 'Method' with that datatype
    /// NOTE: there are a few restrictions while using self in a function
    /// 1. self can only be used inside impl blocks
    /// 2. if a function contains self in it's signature self has to be the first parameter
    /// 3. the self parameter does not need to have any addition type information
    /// example: self    .     foo
    ///          ^-instance ptr  ^-member field
    This,

    /// function call e.g. foo(-42, 1, 1)
    /// example: foo    (-42,     10)
    ///          ^-callee ^-arg0  ^-arg1
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
    LShift,
    RShift,

    And,
    Or,
    Xor,

    BWAnd,
    BWOr,
    BWXor,

    EqEq,
    NotEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
}

#[derive(Debug)]
pub enum UnaryOp {
    Ref,
    Deref,
    Minus,
    Not,
}

#[derive(Debug)]
pub struct Ident {
    pub data: String,
    pub span: Span,
}

pub type PathSegment = Ident;

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}


impl TryFrom<Token> for BinOp {
    type Error = Diagnostic;
    fn try_from(t: Token) -> Result<Self, Self::Error> {
        match t.kind {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Star => Ok(Self::Multiply),
            TokenKind::Slash => Ok(Self::Divide),
            TokenKind::Mod => Ok(Self::Mod),
            TokenKind::LShift => Ok(Self::LShift),
            TokenKind::RShift => Ok(Self::RShift),
            TokenKind::Ref => Ok(Self::BWAnd),
            TokenKind::Sep => Ok(Self::BWOr),
            TokenKind::Caret => Ok(Self::BWXor),
            TokenKind::And => Ok(Self::And),
            TokenKind::Or => Ok(Self::Or),
            TokenKind::Xor => Ok(Self::Xor),

            TokenKind::EqEq => Ok(Self::EqEq),
            TokenKind::NotEq => Ok(Self::NotEq),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEq => Ok(Self::GreaterEq),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEq => Ok(Self::LessEq),
            _ => {
                let msg = format!("Failed to covert token: {:?} to binary operator", t);
                let sp = Some(t.span.clone());
                let diag = Diagnostic::builder()
                    .lvl(Level::Internal(sp))
                    .msg(msg)
                    .note(
                        "This is most likely an issue in the parser, make sure you only try to
                        convert a token if you can guarantee the conversion never fails.",
                    )
                    .build();
                Err(diag)
            }
        }
    }
}

impl TryFrom<Token> for UnaryOp {
    type Error = Diagnostic;
    fn try_from(t: Token) -> Result<Self, Self::Error> {
        match t.kind {
            TokenKind::Bang => Ok(Self::Not),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Star => Ok(Self::Deref),
            TokenKind::Ref => Ok(Self::Ref),
            _ => {
                let sp = Some(t.span.clone());
                let msg = format!("Failed to covert token: {:?} to unary operator", t);
                let diag = Diagnostic::builder()
                    .lvl(Level::Internal(sp))
                    .msg(msg)
                    .note(
                        "This is most likely an issue in the parser, make sure you only try to
                        convert a token if you can guarantee the conversion never fails.",
                    )
                    .build();
                Err(diag)
            }
        }
    }
}
