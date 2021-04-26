use derivative::*;
use parse_display::Display;

use konrad_span::span::Span;

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    span: Span,
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

    /// just like a normal binary expression but only used for logical expressions
    /// example: a      &&    b
    ///          ^-rhs  ^-op  ^-lhs
    Cmp {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: CmpOp,
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
}

#[derive(Debug)]
pub enum CmpOp {
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

#[derive(PartialEq, Debug, Clone, Display)]
pub enum Lit {
    // NOTE(Simon): we store all numbers as positives. The sign get's encoded into the AST
    #[display("{}")]
    Num(u128),
    #[display("{}")]
    Float(f64),
    #[display("{}")]
    Text(String),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
pub enum RangeKind {
    #[display("..=")]
    Inclusive,
    #[display("..")]
    Exclusive,
}
