use std::{convert::TryFrom, path::PathBuf};

use derivative::*;
use konrad_err::diagnostic::*;
use konrad_lexer::token::*;
use konrad_span::span::Span;

#[derive(Debug)]
pub struct Programm {
    pub files: Vec<FileNode>,
}

#[derive(Debug)]
pub struct FileNode {
    pub path:  PathBuf,
    pub decls: Vec<Decl>,
}

impl FileNode {
    pub const fn new(path: PathBuf) -> Self { Self { path, decls: vec![] } }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DeclKind {
    Directive(Directive),
    Enum {
        name:     Ident,
        variants: Vec<EnumVariant>,
    },
    Struct {
        name:    Ident,
        members: Vec<StructMember>,
    },
    Const {
        name:  Ident,
        value: Expr,
    },
    Fn {
        sig:  FnSig,
        body: Box<Block>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnSig {
    pub name:   Ident,
    pub args:   Vec<FnArgument>,
    pub ret_ty: Ty,
    pub span:   Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnArgument {
    pub name: Ident,
    pub ty:   Ty,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span:  Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub kind: VariantKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    /// Expr stmt terminated with a ;
    Expr(Expr),

    /// Expr stmt not terminated with a ;
    /// example
    /// a := { 42 }
    /// here value gets assinged the value of 42 as it is the last expression in
    /// the block and not terminated with a semicolon.
    BlockValue(Expr),

    For {
        var:  Pat,
        init: Expr,
    },
    While {
        cond: Expr,
        body: Block,
    },

    WhileAssign {
        pat:  Pat,
        cond: Expr,
        body: Block,
    },
    Loop {
        body: Block,
    },
    If(IfStmt),
    ConstIf(IfStmt),
    IfAssign {
        pat:           Pat,
        cond:          Expr,
        else_branches: Vec<ElseAssignBranch>,
        final_branch:  Option<Block>,
    },
    Break {
        label: Option<Ident>,
    },
    Continue {
        label: Option<Ident>,
    },
    Return(Option<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseAssignBranch {
    pub pat:  Pat,
    pub cond: Expr,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseBranch {
    pub cond: Expr,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStmt {
    pub cond:          Expr,
    pub body:          Block,
    pub else_branches: Vec<ElseBranch>,
    pub final_branch:  Option<Block>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VariantKind {
    SimpleConstant(Ident),
    Tuple { name: Ident, elems: Vec<Ty> },
    Struct { name: Ident, members: Vec<StructMember> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructMember {
    pub name: Ident,
    pub ty:   Ty,
    pub span: Span,
}

// NOTE(Simon): This is just how we represent types in the ast after parsing.
// NOTE(Simon): If you wan't to work on the typechecker you have to look in the
// typechecking module
#[derive(Debug, PartialEq, Eq)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TyKind {
    /// example: `(u32, u32)`
    Tup(Vec<Ty>),

    /// example: Foo
    Ident(Ident),

    /// example: `Foo<Bar, u8>`
    /// `Foo` is the base
    /// `Bar` and `u8` are the generic bounds for the type `Foo`
    Generic { base: Ident, bounds: Vec<Ty> },
}

impl Ty {
    pub const fn default_unit_type(span: Span) -> Self {
        Self {
            kind: TyKind::Tup(vec![]),
            span,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Directive {
    pub kind: DirectiveKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DirectiveKind {
    Tag(DirectiveName),
    MetaFunc {
        name: DirectiveName,
        args: Option<Vec<Token>>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum DirectiveName {
    Use,
    Custom(Ident),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportPath {
    pub base_path: Path,
    pub qualifier: ImportQualifier,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImportQualifier {
    ItemList(Vec<Ident>),
    WildCard,
    JustPath,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Derivative, PartialEq, Eq)]
#[derivative(Debug)]
pub enum ExprKind {
    /// normal binary expression, only used for numeric expressions
    /// example: a      +     42
    ///          ^-rhs  ^-op  ^-lhs
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op:  BinOp,
    },

    /// one sided expression
    /// example: -    3
    ///          ^-op ^-rhs
    Unary {
        rhs: Box<Expr>,
        op:  UnaryOp,
    },

    /// struct literals are used to initialize objects with values
    /// example: Person {name: "Torben"}
    ///          ^-name  ^^^^^^^^^^^^^^- member with name and init expr
    Struct {
        path:   Path,
        fields: Vec<(Ident, Expr)>,
    },

    /// a tuple expression is just a collection of other expressions
    /// example: (20,    20)
    ///           ^-expr ^-expr
    #[derivative(Debug = "transparent")]
    Tup(Vec<Expr>),

    /// variable reference, possibly containing `::` to refer to types in other
    /// moduels example: foo::bar
    ///          ^^^-segment
    #[derivative(Debug = "transparent")]
    Path(Path),

    /// Enum variant where the specific type has to be infered
    /// example: .Some(42)
    ///           ^^^^-variant
    /// should desugar to:
    ///          Option::Some(42)
    InferedEnum {
        variant: Ident,
        pat:     Option<Box<Pat>>,
    },

    Var(Ident),

    /// used to represent all sorts of index expressions
    /// example: foo[     expr     ]
    ///          ^-callee ^index
    Index {
        callee: Box<Expr>,
        index:  Box<Expr>,
    },

    #[derivative(Debug = "transparent")]
    Array(ArrayInit),

    /// a range pattern
    /// example: 0   ..   10
    ///           ^-start ^-end
    Range {
        lo:   Box<Expr>,
        hi:   Box<Expr>,
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

    /// refers to a object instance and can be used to refer to that instance
    /// and it's member fields e.g. (selbst.foo) if a function contains self
    /// as an argument in it's signature it automatically becomes an associated
    /// 'Method' with that datatype NOTE: there are a few restrictions while
    /// using self in a function 1. self can only be used inside impl blocks
    /// 2. if a function contains self in it's signature self has to be the
    /// first parameter 3. the self parameter does not need to have any
    /// addition type information example: self    .     foo
    ///          ^-instance ptr  ^-member field
    This,

    /// function call e.g. foo(-42, 1, 1)
    /// example: foo    (-42,     10)
    ///          ^-callee ^-arg0  ^-arg1
    Call {
        callee: Box<Expr>,
        args:   Vec<Expr>,
    },
    /// Blocks can act like values in this language
    /// example: foo := { 42 };
    /// If you don't terminate an expression with a semicolon it becomes the
    /// value of the block
    Block(Box<Block>),
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Ref,
    Deref,
    Minus,
    Not,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ArrayInit {
    /// list initializers are used to initialize arrays with values
    /// example: [1, 2, 3, 4, 5]
    ///           ^-create new array with values from 1 to including 5
    List(Vec<Expr>),

    /// fill initializers create a new array of a certain len by repeating an
    /// expression example: [0; 10]
    ///           ^-creates an array of len 10 filled with 0
    Filler { val: Box<Expr>, len: Box<Expr> },
}

#[derive(Derivative, Clone, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Ident {
    pub data: String,

    //#[derivative(Debug = "ignore")]
    pub span: Span,
}

#[derive(Derivative, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Pat {
    pub kind: PatKind,

    //#[derivative(Debug = "ignore")]
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PatKind {
    /// The Or pattern must match at least 1 subpattern
    /// example: .Some(42) | .Some(0) == .Some(42)
    Or(Vec<Pat>),

    /// The expr pattern matches if and only if both patters are identical
    /// example: .Some(42) == .Some(42)
    Expr(Expr),

    /// The wildcard pattern matches against every other pattern
    /// wildcard syntax: _
    /// example: .Some(_) == .Some(42)
    WildCard,
}

pub type PathSegment = Ident;

#[derive(Derivative, Clone, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,

    //#[derivative(Debug = "ignore")]
    pub span: Span,
}

impl Path {
    pub fn len(&self) -> usize { self.segments.len() }

    pub fn is_empty(&self) -> bool { self.segments.is_empty() }

    pub fn first(&self) -> Option<&PathSegment> { self.segments.first() }

    pub fn last(&self) -> Option<&PathSegment> { self.segments.last() }
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
                let sp = Some(t.span);
                let diag = Diagnostic::builder()
                    .lvl(Level::Internal(sp))
                    .msg(msg)
                    .note(
                        "This is most likely an issue in the parser, make sure you only try to
                        convert a token if you can guarantee the conversion never fails.",
                    )
                    .build();
                Err(diag)
            },
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
            },
        }
    }
}
