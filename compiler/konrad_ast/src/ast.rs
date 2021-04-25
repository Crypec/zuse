//use konrad_span::span::Span;
use parse_display::Display;

#[derive(PartialEq, Debug, Clone, Display)]
pub enum Lit {
    // NOTE(Simon): we interpret all numbers as positive. The sign get's encoded into the AST
    #[display("{}")]
    Num(u128),
    #[display("{}")]
    Float(f64),
    #[display("{}")]
    Text(String),
}
