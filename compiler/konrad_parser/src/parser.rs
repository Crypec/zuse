use konrad_ast::ast::*;
use konrad_err::diagnostic::*;
use konrad_lexer::token::*;
use konrad_span::span::*;
use std::convert::TryInto;

type PResult<T> = Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Parser {
    cursor: usize,
    buf: Vec<Token>,
}

macro_rules! __parse_bin_expr_impl {
    ($name:ident, $inner:ident, $cond:pat) => {
        fn $name(&mut self) -> PResult<Expr> {
            let mut lhs = self.$inner()?;
            #[allow(unused_parens)]
            while let $cond = self.peek()?.kind {
                let op = self.next_t()?.try_into()?;
                let rhs = self.$inner()?;
                let span = lhs.span.merge(&rhs.span);
                lhs = Expr {
                    kind: ExprKind::Binary {
                        lhs: box lhs,
                        op,
                        rhs: box rhs,
                    },
                    span,
                };
            }
            Ok(lhs)
        }
    };
}

impl Parser {
    pub fn new(buf: Vec<Token>) -> Self {
        Self { cursor: 0, buf }
    }

    fn parse_expr(&mut self) -> PResult<Expr> {
        todo!()
    fn parse_decl(&mut self) -> PResult<Decl> {
        let name = self.parse_ident("expected name of top level declaration")?;
        self.eat(
            TokenKind::ColCol,
            "expected `::` between name of declaration and type",
        )?;
        match self.peek_kind() {
            _ => self.parse_const_decl(name),
        }
    }

    fn parse_const_decl(&mut self, name: Ident) -> PResult<Decl> {
        let value = self.parse_expr()?;
        let span = self
            .eat(TokenKind::Semi, "expected `;` after const declaration")?
            .span
            .merge(&name.span);
        Ok(Decl {
            kind: DeclKind::Const { name, value },
            span,
        })
    }
    }

    __parse_bin_expr_impl!(parse_or, parse_and, TokenKind::Or);
    __parse_bin_expr_impl!(parse_and, parse_eq, TokenKind::And);
    __parse_bin_expr_impl!(parse_eq, parse_cmp, (TokenKind::EqEq | TokenKind::NotEq));
    __parse_bin_expr_impl!(
        parse_cmp,
        parse_term,
        (TokenKind::Greater | TokenKind::GreaterEq | TokenKind::Less | TokenKind::LessEq)
    );
    __parse_bin_expr_impl!(
        parse_term,
        parse_factor,
        (TokenKind::Plus | TokenKind::Minus)
    );
    __parse_bin_expr_impl!(
        parse_factor,
        parse_unary,
        (TokenKind::Star | TokenKind::Slash)
    );

    fn parse_unary(&mut self) -> PResult<Expr> {
        todo!()
    }

    fn eat(&mut self, expected: TokenKind, note: &str) -> PResult<&Token> {
        match self.next_t() {
            Ok(t) if t.kind == expected => Ok(t),
            Ok(t) => {
                let msg = format!(
                    "Expected the following token: `{}` at this point instead of `{}`",
                    expected, t.kind
                );
                let diag = Diagnostic::builder()
                    .lvl(Level::Error(t.span.clone()))
                    .msg(msg)
                    .note(note)
                    .build();
                Err(diag)
            }
            Err(mut diag) => {
                let note = format!("Expected the following token: `{}` at this point", expected,);
                diag.add_note(note);
                Err(diag)
            }
        }
    }

    fn peek(&self) -> PResult<&Token> {
        self.peek_n(1)
    }

    fn peek_n(&self, n: usize) -> PResult<&Token> {
        let n = n.saturating_sub(1);
        let token = self.buf.get(self.cursor + n);
        match token {
            Some(t) => Ok(t),
            None => {
                let sp = self.get_last_span()?;
                let diag = Diagnostic::builder()
                    .lvl(Level::Error(sp))
                    .msg("Unexpected EOF")
                    .note("tried to peek too far into the future")
                    .build();
                Err(diag)
            }
        }
    }

    fn next_t(&mut self) -> PResult<&Token> {
        let cursor = self.cursor;
        self.cursor += 1;
        match self.buf.get(cursor) {
            Some(t) => Ok(t),
            None => {
                let sp = self.get_last_span()?;
                let diag = Diagnostic::builder()
                    .lvl(Level::Error(sp))
                    .msg("Unexpected EOF")
                    .build();
                Err(diag)
            }
        }
    }

    fn get_last_span(&self) -> PResult<Span> {
        match self.buf.last() {
            Some(t) => Ok(t.span.clone()),
            None => {
                let diag = Diagnostic::builder()
                    .lvl(Level::Internal(None))
                    .msg("Parser :: failed to get last span")
                    .note("Did you try to parse an empty file?")
                    .build();
                Err(diag)
            }
        }
    }
}
