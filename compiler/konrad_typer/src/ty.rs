use konrad_span::span::*;

#[derive(Debug, PartialEq, Eq)]
pub struct Ty {
    kind: TyKind,
    span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TyKind {
    Unit,
}
