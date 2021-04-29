use crate::token::*;
use konrad_ast::ast::{Lit, RangeKind};
use konrad_err::diagnostic::*;
use konrad_span::span::*;
use std::path::PathBuf;

type LexResult<T> = Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Lexer {
    pos: Position,
    path: PathBuf,
    cursor: usize,
    buf: Vec<char>,
}

impl Lexer {
    pub fn new<T: Into<String>, P: Into<PathBuf>>(buf: T, path: P) -> Self {
        Self {
            pos: Position::default(),
            path: path.into(),
            buf: buf.into().chars().collect(),
            cursor: 0,
        }
    }

    pub fn scan_token(&mut self) -> LexResult<Token> {
        let start = self.cursor;

        self.pos.reset_pos_for_new_token();

        let c = self.next_char()?;
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
                .map_if(|p| p == '=', TokenKind::NotEq)
                .unwrap_or(TokenKind::Bang),
            '<' if self.peek() == Some('=') => {
                self.inc_cursor(1);
                TokenKind::LessEq
            }
            '<' if self.peek() == Some('<') => {
                self.inc_cursor(1);
                TokenKind::RShift
            }
            '<' => TokenKind::Less,
            '>' if self.peek() == Some('=') => {
                self.inc_cursor(1);
                TokenKind::GreaterEq
            }
            '>' if self.peek() == Some('>') => {
                self.inc_cursor(1);
                TokenKind::LShift
            }
            '>' => TokenKind::Greater,
            '|' => self
                .map_if(|c| c == '|', TokenKind::Or)
                .unwrap_or(TokenKind::Sep),
            '&' => self
                .map_if(|c| c == '|', TokenKind::And)
                .unwrap_or(TokenKind::Ref),
            '-' => self
                .map_if(|p| p == '>', TokenKind::ThinArrow)
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
                    self.inc_cursor(2);
                    TokenKind::Range(RangeKind::Inclusive)
                }
                (Some('.'), _) => {
                    self.inc_cursor(1);
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

        let span = self.get_current_span();
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
        self.inc_cursor(prefix_len);
        self.advance_while(|c| matches(c) || c == '_')?;

        let num_start = start + prefix_len;
        let num_str = self.sub_string(num_start, self.cursor).replace("_", "");
        match u128::from_str_radix(&num_str, radix) {
            Ok(n) => Ok(TokenKind::Lit(Lit::Num(n))),
            Err(e) => {
                let sp = self.get_current_span();
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

    fn get_current_span(&self) -> Span {
        // subtract 1 to make the range inclusive
        let end_col = self.pos.end_col.saturating_sub(1);

        let start_lc = LineColumn::new(self.pos.start_line, self.pos.start_col);
        let end_lc = LineColumn::new(self.pos.current_line, end_col);
        Span::new(start_lc, end_lc, self.path.clone())
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
        self.inc_cursor(1);
        match self.buf.get(cursor).copied() {
            Some(c) => {
                // FIXME(Simon): This is not going to hold up too well for different
                // FIXME(Simon): operating systems and the unicode standart
                if c == '\n' {
                    self.pos.update_new_line();
                }
                Ok(c)
            }
            None => {
                let sp = self.get_current_span();
                let diag = Diagnostic::builder()
                    .lvl(Level::Error)
                    .msg("Unexepected End of File")
                    .span(sp)
                    .build();
                Err(diag)
            }
        }
    }

    fn map_if<F>(&mut self, p: F, tk: TokenKind) -> Option<TokenKind>
    where
        F: Fn(char) -> bool,
    {
        match self.peek() {
            Some(c) => {
                if p(c) {
                    self.inc_cursor(1);
                    Some(tk)
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
    fn inc_cursor(&mut self, n: usize) {
        self.cursor += n;
        self.pos.end_col += n;
    }

    fn has_next(&self) -> bool {
        self.buf.len() > self.cursor
    }
}

#[derive(Debug, Default)]
struct Position {
    start_line: usize,
    current_line: usize,
    start_col: usize,
    end_col: usize,
}

impl Position {
    fn reset_pos_for_new_token(&mut self) {
        self.start_line = self.current_line;
        self.start_col = self.end_col;
    }

    fn update_new_line(&mut self) {
        self.current_line += 1;
        self.end_col = 0;
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
