use crate::token::*;
use konrad_ast::ast::{Lit, RangeKind};
use konrad_err::diagnostic::*;
use konrad_span::span::Span;
use std::path::PathBuf;

type LexResult<T> = Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Lexer {
    cursor: usize,
    buf: Vec<char>,
    path: PathBuf,
}

impl Lexer {
    pub fn new<T: Into<String>, P: Into<PathBuf>>(buf: T, path: P) -> Self {
        Self {
            cursor: 0,
            buf: buf.into().chars().collect(),
            path: path.into(),
        }
    }

    pub fn scan_token(&mut self) -> LexResult<Token> {
        let start = self.cursor;
        let c = self.next_char()?;
        dbg!(&c);
        let kind = match c {
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semi,
            '$' => TokenKind::Dollar,
            '_' => TokenKind::WildCard,
            '+' => TokenKind::Plus,
            '%' => TokenKind::Mod,
            '^' => TokenKind::Caret,
            '*' => TokenKind::Star,
            '#' => TokenKind::Hash,
            '!' => self
                .map_if(|p| p == '=', || TokenKind::NotEq)
                .unwrap_or(TokenKind::Bang),
            '<' => self
                .map_if(|p| p == '=', || TokenKind::GreaterEq)
                .unwrap_or(TokenKind::Greater),
            '>' => self
                .map_if(|p| p == '=', || TokenKind::LessEq)
                .unwrap_or(TokenKind::Less),
            '|' => self
                .map_if(|c| c == '|', || TokenKind::Or)
                .unwrap_or(TokenKind::Sep),
            '&' => self
                .map_if(|c| c == '|', || TokenKind::And)
                .unwrap_or(TokenKind::Ref),
            '-' => self
                .map_if(|p| p == '>', || TokenKind::ThinArrow)
                .unwrap_or(TokenKind::Minus),
            ':' => match self.peek() {
                Some('=') => {
                    self.next_char()?;
                    TokenKind::ColEq
                }
                Some(':') => {
                    self.next_char()?;
                    TokenKind::ColCol
                }
                _ => TokenKind::Col,
            },
            '=' => match self.peek() {
                Some('=') => {
                    self.next_char()?;
                    TokenKind::EqEq
                }
                Some('<') => {
                    self.next_char()?;
                    TokenKind::LessEq
                }
                Some('>') => {
                    self.next_char()?;
                    TokenKind::FatArrow
                }
                _ => TokenKind::Eq,
            },
            '.' => match (self.peek_n(1), self.peek_n(2)) {
                (Some('.'), Some('=')) => {
                    self.cursor += 2;
                    TokenKind::Range(RangeKind::Inclusive)
                }
                (Some('.'), _) => {
                    self.cursor += 1;
                    TokenKind::Range(RangeKind::Exclusive)
                }
                (_, _) => TokenKind::Dot,
            },
            // TODO(Simon): handle multiline comments
            '/' if self.peek() == Some('/') => {
                // if we find a comment we can skip until we find a new line
                self.advance_while(|c| c != '\n')?;
                let comment = self.sub_string(start, self.cursor);
                TokenKind::SLComment(comment)
            }
            '/' => TokenKind::Slash,
            '"' => self.lex_string(start)?,
            '0' if self.peek() == Some('b') => {
                self.lex_num(start, 2, 2, |c| matches!(c, '0' | '1'))?
            }
            '0' if self.peek() == Some('o') => {
                self.lex_num(start, 8, 2, |c| matches!(c, '0'..='7'))?
            }
            '0' if self.peek() == Some('x') => {
                self.lex_num(start, 16, 2, |c| matches!(c, '0'..='F' | '0'..='f'))?
            }
            '0'..='9' => self.lex_num(start, 10, 0, |c| matches!(c, '0'..='9'))?,
            _ if c.is_whitespace() => self.eat_whitespace()?,
            _ => self.lex_ident(start)?,
        };

        let span = Span::new(start, self.cursor, self.path.clone());
        Ok(Token { kind, span })
    }

    fn lex_num<F>(
        &mut self,
        start: usize,
        radix: u32,
        prefix_len: usize,
        matches: F,
    ) -> LexResult<TokenKind>
    where
        F: Fn(char) -> bool,
    {
        self.cursor += prefix_len;
        self.advance_while(|c| matches(c) || c == '_')?;

        let num_start = start + prefix_len;
        let num_str = self.sub_string(num_start, self.cursor).replace("_", "");
        match u128::from_str_radix(&num_str, radix) {
            Ok(n) => Ok(TokenKind::Lit(Lit::Num(n))),
            Err(e) => {
                let sp = Span::new(start, self.cursor, self.path.clone());
                let diag = Diagnostic::builder()
                    .lvl(Level::Internal)
                    .msg(format!(
                        "Failed to parse number literal with radix {}",
                        radix
                    ))
                    .span(sp)
                    .note(&format!("{:?}", e))
                    .build();
                Err(diag)
            }
        }
    }

    fn lex_ident(&mut self, start: usize) -> LexResult<TokenKind> {
        self.advance_while(|p| p.is_alphanumeric() || p == '_')?;
        let lexeme = self.sub_string(start, self.cursor);
        Ok(str::parse::<Keyword>(&lexeme)
            .map(TokenKind::Keyword)
            .unwrap_or(TokenKind::Ident(lexeme)))
    }

    fn lex_string(&mut self, start: usize) -> LexResult<TokenKind> {
        self.advance_while(|p| p != '"')?;
        self.next_char()?; // skip "
        let lexeme = self.sub_string(start + 1, self.cursor - 1);
        Ok(TokenKind::Lit(Lit::Text(lexeme)))
    }

    fn eat_whitespace(&mut self) -> LexResult<TokenKind> {
        self.advance_while(|c| c.is_whitespace())?;
        Ok(TokenKind::WhiteSpace)
    }

    fn advance_while<F>(&mut self, f: F) -> LexResult<()>
    where
        F: Fn(char) -> bool,
    {
        while let Some(c) = self.peek() {
            match f(c) {
                true => self.next_char()?,
                false => break,
            };
        }
        Ok(())
    }

    fn get_current_span(&self, start: usize) -> Span {
        Span::new(start, self.cursor, self.path.clone())
    }

    fn peek(&self) -> Option<char> {
        self.peek_n(1)
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        let n = n.saturating_sub(1);
        self.buf.get(self.cursor + n).copied()
    }

    fn next_char(&mut self) -> LexResult<char> {
        let cursor = self.cursor;
        self.cursor += 1;
        match self.buf.get(cursor) {
            Some(c) => Ok(*c),
            None => {
                let sp = self.get_current_span(cursor);
                let diag = Diagnostic::builder()
                    .lvl(Level::Error)
                    .msg("Unexepected End of File")
                    .span(sp)
                    .build();
                Err(diag)
            }
        }
    }

    fn map_if<F, T>(&mut self, p: F, tk: T) -> Option<TokenKind>
    where
        F: Fn(char) -> bool,
        T: Fn() -> TokenKind,
    {
        match self.peek() {
            Some(c) => {
                if p(c) {
                    self.next_char().expect("failed to get next char");
                    Some(tk())
                } else {
                    None
                }
            }
            None => None,
        }
    }

    fn sub_string(&self, start: usize, end: usize) -> String {
        debug_assert!(
            start <= end,
            "start of substring can't be behind end {}..{}",
            start,
            end
        );
        self.buf[start..end].iter().collect()
    }

    fn has_next(&self) -> bool {
        self.buf.len() > self.cursor
    }
}

impl Iterator for Lexer {
    type Item = LexResult<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.has_next() {
            Some(self.scan_token())
        } else {
            None
        }
    }
}
