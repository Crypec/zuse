use crate::token::*;
use konrad_span::span::*;
use std::path::PathBuf;

use konrad_session::session::*;

#[derive(Debug)]
pub struct Lexer<'s> {
    pos: Position,
    path: PathBuf,

    cursor: usize,
    buf: Vec<char>,

    sess: &'s mut Session,
}

impl<'s> Lexer<'s> {
    pub fn new<T: Into<String>, P: Into<PathBuf>>(buf: T, path: P, sess: &'s mut Session) -> Self {
        Self {
            pos: Position::default(),
            path: path.into(),

            buf: buf.into().chars().collect(),
            cursor: 0,
            sess,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::with_capacity(1000);
        while self.has_next() {
            tokens.push(self.scan_token().unwrap());
        }
        tokens
    }

    fn scan_token(&mut self) -> Option<Token> {
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
            '*' => TokenKind::Star,
            '#' => TokenKind::Hash,
            '^' => self
                .map_if(|p| p == '^', TokenKind::Xor)
                .unwrap_or(TokenKind::Caret),
            '!' => self
                .map_if(|p| p == '=', TokenKind::NotEq)
                .unwrap_or(TokenKind::Bang),
            '<' if self.peek() == Some('=') => {
                self.inc_cursor(1);
                TokenKind::LessEq
            }
            '<' if self.peek() == Some('<') => {
                self.inc_cursor(1);
                TokenKind::LShift
            }
            '<' => TokenKind::Less,
            '>' if self.peek() == Some('=') => {
                self.inc_cursor(1);
                TokenKind::GreaterEq
            }
            '>' if self.peek() == Some('>') => {
                self.inc_cursor(1);
                TokenKind::RShift
            }
            '>' => TokenKind::Greater,
            '|' => self
                .map_if(|c| c == '|', TokenKind::Or)
                .unwrap_or(TokenKind::Sep),
            '&' => self
                .map_if(|c| c == '&', TokenKind::And)
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
                (..) => TokenKind::Dot,
            },
            // TODO(Simon): handle multiline comments
            '/' if self.peek() == Some('/') => {
                // if we find a comment we can skip until we find a new line
                self.advance_while(|c| c != '\n')?;
                let comment = self.sub_string(start, self.cursor);
                TokenKind::SLComment(comment)
            }
            '/' if self.peek() == Some('*') => {
                // skip start of comment
                self.next_char()?;
                self.next_char()?;

                let opening_span = self.get_current_span();

                loop {
                    match (self.peek_n(1), self.peek_n(2)) {
                        (Some('*'), Some('/')) => {
                            // skip end of comment
                            self.next_char()?;
                            self.next_char()?;
                            break;
                        }
                        (Some(_), Some(_)) => self.next_char()?,
                        _ => {
                            self.sess
                                .span_err("Unclosed multiline comment", opening_span);
                            return None;
                        }
                    };
                }
                let comment = self.sub_string(start, self.cursor);
                TokenKind::MLComment(comment)
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
        Some(Token { kind, span })
    }

    /// TODO(Simon): Implement float parsing
    fn lex_num<F>(
        &mut self,
        start: usize,
        radix: u32,
        prefix_len: usize,
        matches: F,
    ) -> Option<TokenKind>
    where
        F: Fn(char) -> bool,
    {
        self.inc_cursor(prefix_len);

        self.advance_while(|c| matches(c) || c == '_')?;

        let num_start = start + prefix_len;
        let num_str = self.sub_string(num_start, self.cursor).replace("_", "");
        match u128::from_str_radix(&num_str, radix) {
            Ok(n) => Some(TokenKind::Lit(Lit::Num(n))),
            Err(e) => {
                let sp = self.get_current_span();
                let msg = format!("Failed to parse number literal with radix {}", radix);
                self.sess.span_err(msg, sp).add_note(format!("{:?}", e));

                // NOTE(Simon): Should we really emit a valid token?
                // NOTE(Simon): This allows use to report more errors in a single run but we have
                // NOTE(Simon): to be careful not to emit false diagnostic based on this token.
                Some(TokenKind::Lit(Lit::Num(0)))
            }
        }
    }

    fn lex_ident(&mut self, start: usize) -> Option<TokenKind> {
        self.advance_while(|p| p.is_alphanumeric() || p == '_')?;
        let lexeme = self.sub_string(start, self.cursor);
        Some(
            str::parse::<Keyword>(&lexeme)
                .map(TokenKind::Keyword)
                .unwrap_or(TokenKind::Ident(lexeme)),
        )
    }

    fn lex_string(&mut self, start: usize) -> Option<TokenKind> {
        self.advance_while(|p| p != '"')?;
        self.next_char()?; // skip "
        let lexeme = self.sub_string(start + 1, self.cursor - 1);
        Some(TokenKind::Lit(Lit::Text(lexeme)))
    }

    fn eat_whitespace(&mut self) -> Option<TokenKind> {
        self.advance_while(|c| c.is_whitespace())?;
        Some(TokenKind::WhiteSpace)
    }

    fn advance_while<F>(&mut self, f: F) -> Option<()>
    where
        F: Fn(char) -> bool,
    {
        while let Some(c) = self.peek() {
            match f(c) {
                true => self.next_char()?,
                false => break,
            };
        }
        Some(())
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

    fn next_char(&mut self) -> Option<char> {
        let cursor = self.cursor;
        self.inc_cursor(1);
        let next_char = self.buf.get(cursor).copied();
        if let Some('\n') = next_char {
            self.pos.update_new_line();
        }
        next_char
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
