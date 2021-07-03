use std::{convert::TryInto, path::PathBuf};

use konrad_ast::*;

#[allow(unused_imports)]
use konrad_err::diagnostic::*;

use konrad_lexer::token::*;
use konrad_session::session::*;

#[allow(unused_imports)]
use konrad_span::span::*;

#[derive(Debug)]
pub struct Parser<'s> {
    cursor: usize,
    // NOTE(Simon): If we use a better container here we could save a lot of clones while advancing
    // NOTE(Simon): through next_t()
    buf:    Vec<Token>,
    sess:   &'s mut Session,
}

macro_rules! __parse_bin_expr_impl {
    ($name:ident, $inner:ident, $cond:pat) => {
        fn $name(&mut self) -> Option<Expr> {
            let mut lhs = self.$inner()?;

            #[allow(unused_parens)]
            while let Some($cond) = self.peek_kind() {
                let op = match self.next_t()?.try_into() {
                    Ok(op) => op,
                    Err(d) => {
                        self.sess.report_diagnostic(d);
                        return None;
                    },
                };
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
            Some(lhs)
        }
    };
}

impl<'s> Parser<'s> {
    __parse_bin_expr_impl!(parse_or, parse_and, TokenKind::Or);

    __parse_bin_expr_impl!(parse_and, parse_eq, TokenKind::And);

    __parse_bin_expr_impl!(parse_eq, parse_cmp, (TokenKind::EqEq | TokenKind::NotEq));

    __parse_bin_expr_impl!(
        parse_cmp,
        parse_term,
        (TokenKind::Greater | TokenKind::GreaterEq | TokenKind::Less | TokenKind::LessEq)
    );

    __parse_bin_expr_impl!(parse_term, parse_factor, (TokenKind::Plus | TokenKind::Minus));

    __parse_bin_expr_impl!(parse_factor, parse_unary, (TokenKind::Star | TokenKind::Slash));

    pub fn new(buf: Vec<Token>, session: &'s mut Session) -> Self {
        Self {
            cursor: 0,
            buf,
            sess: session,
        }
    }

    pub fn parse(&mut self) -> Option<Decl> { self.parse_decl() }

    pub fn parse_entrie_file(&mut self, path: PathBuf) -> FileNode {
        let mut file_node = FileNode::new(path);
        while self.has_next() {
            match self.parse_decl() {
                Some(d) => file_node.decls.push(d),
                None => self.sync_parser_state(),
            }
        }
        file_node
    }

    // TODO(Simon): Currently syncing the parser is not very granular because we are
    // always TODO(Simon): searching for the next struct, enum or impl block.
    // All other unreleated tokens TODO(Simon): simply get skipped! In order to
    // provide a better user experience we should look TODO(Simon): into how we
    // can make this more nuanced and maybe look at free functions or even
    // TODO(Simon): single .
    fn sync_parser_state(&mut self) {
        while let Some(kind) = self.peek_kind() {
            if let TokenKind::Ident(_) = kind {
                if let Some(TokenKind::Keyword(Keyword::Enum | Keyword::Struct | Keyword::Impl)) = self.peek_n_kind(3) {
                    return;
                }
            }
            // NOTE(Simon): This should never panic because we only get here if we can
            // NOTE(Simon): successfully peek one token ahead.
            self.next_t().expect("failed to advance parser in sync parser state");
        }
    }

    fn parse_decl(&mut self) -> Option<Decl> {
        let directive = self.peek_next_parsing_directive()?;
        match directive {
            NextDeclParseDirective::Func => self.parse_fn_decl(),
            NextDeclParseDirective::Enum => self.parse_enum_decl(),
            NextDeclParseDirective::Struct => self.parse_struct_decl(),
            NextDeclParseDirective::ConstDecl => self.parse_const_decl(),
            NextDeclParseDirective::HashDirective => self.parse_directive_decl(),
        }
    }

    fn parse_struct_decl(&mut self) -> Option<Decl> {
        let name = self.parse_ident("expected name of struct")?;
        self.eat(
            TokenKind::ColCol,
            "expected `::` seperater between `struct` keyword and name of struct",
        )?;
        self.eat(
            TokenKind::Keyword(Keyword::Struct),
            "You need to use the `struct` keyword to declare a struct type",
        )?;
        self.eat(TokenKind::LBrace, "expected the opening `{` before the struct body")?;

        let mut members = vec![];
        while self.peek_kind()? != TokenKind::RBrace {
            members.push(self.parse_struct_member()?);
            if self.peek_kind()? != TokenKind::RBrace {
                self.eat(TokenKind::Comma, "expected a `,` to seperate struct members")?;
            }
        }
        let span = self
            .eat(TokenKind::RBrace, "expected a closing `}` after struct body")?
            .span
            .merge(&name.span);
        Some(Decl {
            kind: DeclKind::Struct { name, members },
            span,
        })
    }

    fn parse_enum_decl(&mut self) -> Option<Decl> {
        let name = self.parse_ident("expected name of enum")?;
        self.eat(
            TokenKind::ColCol,
            "expected `::` between name of enum declaration and body",
        )?;
        self.eat(TokenKind::Keyword(Keyword::Enum), "expected `enum` keyword")?;
        self.eat(TokenKind::LBrace, "expected `{` to enclose body of enum")?;

        let mut variants = vec![];
        while self.peek_kind()? != TokenKind::RBrace {
            variants.push(self.parse_enum_variant()?);
            if self.peek_kind()? != TokenKind::RBrace {
                self.eat(TokenKind::Comma, "expected a `,` to seperate enum variants")?;
            }
        }
        let span = self
            .eat(TokenKind::RBrace, "expected a closing `}` after enum body")?
            .span
            .merge(&name.span);
        Some(Decl {
            kind: DeclKind::Enum { name, variants },
            span,
        })
    }

    fn parse_enum_variant(&mut self) -> Option<EnumVariant> {
        let name = self.parse_ident("expected name of variant")?;
        match self.peek_kind()? {
            TokenKind::LParen => {
                let opening = self.next_t()?.span; // consume opening LParen

                let mut elems = vec![];
                while self.peek_kind()? != TokenKind::RParen {
                    let t = self.parse_ty("expected tuple elem")?;
                    elems.push(t);

                    if self.peek_kind()? != TokenKind::RParen {
                        self.eat(
                            TokenKind::Comma,
                            "expected a comma to seperate tuple elems in enum variant",
                        )?;
                    }
                }
                let span = self.next_t()?.span.merge(&opening); // consume closing RParen

                Some(EnumVariant {
                    kind: VariantKind::Tuple { name, elems },
                    span,
                })
            },
            TokenKind::LBrace => {
                let opening = self.next_t()?.span;

                let mut members = vec![];
                while self.peek_kind()? != TokenKind::RBrace {
                    members.push(self.parse_struct_member()?);
                    self.eat(TokenKind::Comma, "expected comma after struct member in enum variant")?;
                }
                let span = self.next_t()?.span.merge(&opening);

                Some(EnumVariant {
                    kind: VariantKind::Struct { name, members },
                    span,
                })
            },
            _ => {
                let span = name.span.clone();
                Some(EnumVariant {
                    kind: VariantKind::SimpleConstant(name),
                    span,
                })
            },
        }
    }

    fn parse_struct_member(&mut self) -> Option<StructMember> {
        let name = self.parse_ident("expected name of struct member")?;
        self.eat(
            TokenKind::Col,
            "expecting a `:` to seperate the name of the structmember from it's type",
        )?;
        let ty = self.parse_ty("expected type of struct Member")?;
        let span = name.span.merge(&ty.span);
        Some(StructMember { name, ty, span })
    }

    fn parse_ty<S: Into<String>>(&mut self, msg: S) -> Option<Ty> {
        let span = self.peek()?.span;
        let ty = self.parse_ty_quiet();
        if ty.is_none() {
            self.sess.span_err(msg, span);
        }
        ty
    }

    fn parse_ty_quiet(&mut self) -> Option<Ty> {
        let (kind, span) = match self.peek_kind()? {
            TokenKind::LParen => {
                let opening = self.next_t()?.span;
                let mut elems = vec![];
                while self.peek_kind()? != TokenKind::RParen {
                    elems.push(self.parse_ty_quiet()?);
                    if self.peek_kind()? != TokenKind::RParen {
                        self.eat(TokenKind::Comma, "expected a comma to seperate a type tuple")?;
                    }
                }
                let span = self.next_t()?.span.merge(&opening);
                (TyKind::Tup(elems), span)
            },
            TokenKind::Ident(_) => {
                let i = self.parse_ident("expected name of type")?;
                if let Some(TokenKind::Less) = self.peek_kind() {
                    let mut bounds = vec![];
                    self.next_t()?;
                    while self.peek_kind()? != TokenKind::Greater {
                        bounds.push(self.parse_ty_quiet()?);
                        if self.peek_kind()? != TokenKind::Greater {
                            self.eat(
                                TokenKind::Comma,
                                "expected a comma to seperate types in a generic argument list",
                            )?;
                        }
                    }
                    let span = self.next_t()?.span.merge(&i.span);
                    (TyKind::Generic { base: i, bounds }, span)
                } else {
                    let span = i.span.clone();
                    (TyKind::Ident(i), span)
                }
            },
            _ => return None,
        };
        Some(Ty { kind, span })
    }

    fn parse_fn_decl(&mut self) -> Option<Decl> {
        let sig = self.parse_fn_sig()?;
        let body = self.parse_block()?;
        let span = sig.span.merge(&body.span);
        Some(Decl {
            kind: DeclKind::Fn { sig, body: box body },
            span,
        })
    }

    fn parse_fn_sig(&mut self) -> Option<FnSig> {
        let name = self.parse_ident("expected function name")?;
        self.eat(
            TokenKind::ColCol,
            "expected `::` to seperate name of function from arguments",
        )?;
        self.eat(TokenKind::LParen, "expected `(` before function arguments")?;
        let mut args = vec![];
        while self.peek_kind()? != TokenKind::RParen {
            let name = self.parse_ident("expected name of function argument")?;
            self.eat(
                TokenKind::Col,
                "expected a `:` to seperate the name of the argument from it's type",
            )?;
            let ty = self.parse_ty("expected type of function argument")?;
            args.push(FnArgument { name, ty })
        }

        let closing = self
            .eat(TokenKind::RParen, "expected closing `)` after function argument list")?
            .span;

        let ret_ty = if self.peek_kind()? == TokenKind::ThinArrow {
            self.next_t()?;
            self.parse_ty("expected return type of function")?
        } else {
            let span = self.peek()?.span.merge(&closing);
            Ty::default_unit_type(span)
        };
        let span = closing.merge(&ret_ty.span);
        Some(FnSig {
            name,
            args,
            ret_ty,
            span,
        })
    }

    fn parse_block(&mut self) -> Option<Block> {
        let start = self.eat(TokenKind::LBrace, "expected `{` to open a new block")?.span;

        let mut stmts = vec![];
        while self.peek_kind()? != TokenKind::RBrace {
            let s = self.parse_stmt()?;
            if let Stmt {
                kind: StmtKind::BlockValue(_),
                span,
            } = &s
            {
                if self.peek_kind()? != TokenKind::RBrace {
                    self.sess
                        .span_err("Unexpected stmt after after block value", span.clone())
                        .add_note("Did you forget to add a terminating semicolon?");
                    return None;
                }
            }
            stmts.push(s);
        }
        let span = self.next_t()?.span.merge(&start); // consome closing RBrace
        Some(Block { stmts, span })
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.peek_kind()? {
            TokenKind::Keyword(Keyword::For) => self.parse_for_loop(),
            TokenKind::Keyword(Keyword::While) => self.parse_while_loop(),
// FIXME(Simon): @CLEAN_PARSE_EXPR_STMT // FIXME(Simon): We should really clean this up! // FIXME(Simon): There are far too many calls to 'parse_expr_stmt'. TokenKind::Ident(_) => {
                for i in 1.. {
                    match self.peek_n_kind(i) {
                        Some(TokenKind::Eq) => return self.parse_assign(),
                        Some(TokenKind::ColEq) => return self.parse_vardef(),
                        Some(TokenKind::Semi) | None => return self.parse_expr_stmt(), // @CLEAN_PARSE_EXPR_STMT
                        _ => continue,
                    }
                }
                self.parse_expr_stmt() // @CLEAN_PARSE_EXPR_STMT
            },
            _ => self.parse_expr_stmt(), // @CLEAN_PARSE_EXPR_STMT
        }
    }

    // FIXME(Simon):
    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expr()?;
        if self.peek_kind()? == TokenKind::Semi {
            let span = self.next_t()?.span.merge(&expr.span);
            Some(Stmt {
                kind: StmtKind::Expr(expr),
                span,
            })
        } else {
            let span = expr.span.clone();
            Some(Stmt {
                kind: StmtKind::BlockValue(expr),
                span,
            })
        }
    }

    fn parse_directive_decl(&mut self) -> Option<Decl> {
        let d = self.parse_directive()?;
        let span = d.span.clone();
        Some(Decl {
            kind: DeclKind::Directive(d),
            span,
        })
    }

    fn parse_directive(&mut self) -> Option<Directive> {
        let start = self.next_t()?.span; // consume # token
        if let Some(TokenKind::LBracket) = self.peek_kind() {
            return self.parse_meta_func_directive(start);
        }
        self.parse_meta_tag_directive(start)
    }

    fn parse_meta_func_directive(&mut self, start: Span) -> Option<Directive> {
        self.next_t()?; // consume opening LBracket
        let name_ident = self.parse_ident("expected name of directive")?;

        let directive_name = Self::match_directive_name(name_ident);

        let args = if let Some(TokenKind::LParen) = self.peek_kind() {
            // TODO(Simon): We have to decide if we really want to allow this kind of
            // unstructured TODO(Simon): input to meta function directives.
            {
                self.next_t()?; // consume opening paren
                let mut input_tokens = vec![];
                while let Some(TokenKind::RParen) = self.peek_kind() {
                    input_tokens.push(self.next_t()?);
                }
                self.next_t()?; // consume closing paren
                Some(input_tokens)
            }
        } else {
            None
        };

        let span = self
            .eat(TokenKind::RBracket, "expected closing `]` of meta func directive")?
            .span
            .merge(&start);

        Some(Directive {
            kind: DirectiveKind::MetaFunc {
                name: directive_name,
                args,
            },
            span,
        })
    }

    fn parse_meta_tag_directive(&mut self, start: Span) -> Option<Directive> {
        let name_ident = self.parse_ident("expected name of directive")?;
        let span = start.merge(&name_ident.span);
        let directive_name = Self::match_directive_name(name_ident);

        Some(Directive {
            kind: DirectiveKind::Tag(directive_name),
            span,
        })
    }

    fn match_directive_name(name: Ident) -> DirectiveName {
        match name.data.as_str() {
            "use" => DirectiveName::Use,
            _ => DirectiveName::Custom(name),
        }
    }

    fn parse_const_decl(&mut self) -> Option<Decl> {
        let name = self.parse_ident("expected name of variable")?;

        match self.peek()? {
            Token {
                kind: TokenKind::ColCol | TokenKind::ColEq,
                ..
            } => self.next_t()?,
            Token { kind: _, span } => {
                self.sess
                    .span_err("expected either `::` or `:=` to declare a global variable", span);
                return None;
            },
        };

        let value = self.parse_expr()?;
        let span = self
            .eat(TokenKind::Semi, "expected `;` after const declaration")?
            .span
            .merge(&name.span);
        Some(Decl {
            kind: DeclKind::Const { name, value },
            span,
        })
    }

    fn parse_assign(&mut self) -> Option<Stmt> {
        let target = self.parse_assign_target()?;
        self.eat(TokenKind::Eq, "expected `=` after assignment target")?;
        let expr = self.parse_expr()?;
        let span = self
            .eat(TokenKind::Semi, "expected terminating `;` after assignment")?
            .span
            .merge(&target.span);

        Some(Stmt {
            kind: StmtKind::Assign { target, expr },
            span,
        })
    }

    fn parse_assign_target(&mut self) -> Option<AssignTarget> {
        todo!();
    }

    fn parse_vardef(&mut self) -> Option<Stmt> {
        let pat = self.parse_pattern()?;
        self.eat(TokenKind::ColEq, "expected := to define variable")?;
        let init = self.parse_expr()?;
        let span = self
            .eat(TokenKind::Semi, "expected terminating `;` after variable definition")?
            .span
            .merge(&pat.span);
        Some(Stmt {
            kind: StmtKind::VarDef { pat, init },
            span,
        })
    }

    fn parse_for_loop(&mut self) -> Option<Stmt> {
        let start = self.next_t()?.span;
        let pat = self.parse_pattern()?;
        self.eat(TokenKind::ColEq, "expected := to define binding in for loop")?;
        let init = self.parse_expr()?;
        let body = self.parse_stmt()?;
        let span = start.merge(&body.span);
        Some(Stmt {
            kind: StmtKind::For {
                pat,
                init,
                body: box body,
            },
            span,
        })
    }

    fn parse_while_loop(&mut self) -> Option<Stmt> {
        let start = self.next_t()?.span; // consume `while` token
        let cond = self.parse_expr()?;
        let body = self.parse_stmt()?;
        let span = start.merge(&body.span);
        Some(Stmt {
            kind: StmtKind::While { cond, body: box body },
            span,
        })
    }

    pub fn parse_expr(&mut self) -> Option<Expr> { self.parse_range() }

    fn parse_range(&mut self) -> Option<Expr> {
        let lhs = self.parse_or()?;
        if let Some(TokenKind::Range(kind)) = self.peek_kind() {
            self.next_t()?;
            let rhs = self.parse_expr()?;
            let span = lhs.span.merge(&rhs.span);
            return Some(Expr {
                kind: ExprKind::Range {
                    lo: box lhs,
                    hi: box rhs,
                    kind,
                },
                span,
            });
        }
        Some(lhs)
    }

    fn parse_unary(&mut self) -> Option<Expr> {
        match self.peek_kind() {
            Some(TokenKind::Bang | TokenKind::Minus | TokenKind::Ref | TokenKind::Star) => {
                let token = self.next_t()?;
                let rhs = self.parse_unary()?;
                let span = token.span.merge(&rhs.span);

                let op = match token.try_into() {
                    Ok(op) => op,
                    Err(d) => {
                        self.sess.report_diagnostic(d);
                        return None;
                    },
                };

                Some(Expr {
                    kind: ExprKind::Unary { rhs: box rhs, op },
                    span,
                })
            },
            Some(TokenKind::Dot) => {
                let start = self.next_t()?.span;
                let variant = self.parse_ident("enum inference allows you to omit the type of enum")?;

                let (pat, span) = match self.peek_kind() {
                    Some(TokenKind::LParen) => {
                        self.next_t()?;

                        let pat = self.parse_pattern()?;
                        let span = self
                            .eat(TokenKind::RParen, "unmatched start and closing delimitars")?
                            .span
                            .merge(&start);
                        (Some(box pat), span)
                    },
                    Some(TokenKind::LBrace) => {
                        self.next_t()?;

                        let pat = self.parse_pattern()?;
                        let span = self
                            .eat(TokenKind::RBrace, "unmatched start and closing delimitars")?
                            .span
                            .merge(&start);
                        (Some(box pat), span)
                    },
                    _ => {
                        let span = start.merge(&variant.span);
                        (None, span)
                    },
                };

                // TODO(Simon): Allow expression in enum variants
                // Example: .Some(42)
                Some(Expr {
                    kind: ExprKind::InferedEnum { variant, pat },
                    span,
                })
            },
            _ => self.parse_call(),
        }
    }

    fn parse_call(&mut self) -> Option<Expr> {
        let mut lhs = self.parse_primary()?;
        loop {
            match self.peek_kind() {
                Some(TokenKind::LParen) => {
                    self.next_t()?;
                    lhs = self.finish_call(lhs)?;
                },
                Some(TokenKind::LBracket) => {
                    lhs = self.parse_index(lhs)?;
                },
                Some(TokenKind::Dot) => {
                    let start = self.next_t()?.span.clone();
                    let name = self.parse_ident("Expected name of struct field")?;
                    let span = name.span.merge(&start);
                    lhs = Expr {
                        kind: ExprKind::Field(box lhs, name),
                        span,
                    };
                },
                _ => break,
            }
        }
        Some(lhs)
    }

    fn finish_call(&mut self, lhs: Expr) -> Option<Expr> {
        let mut args = vec![];
        while self.peek_kind() != Some(TokenKind::RParen) {
            let arg = self.parse_expr()?;
            args.push(arg);
            if self.peek_kind() != Some(TokenKind::RParen) {
                self.eat(TokenKind::Comma, "expected comma after function argument")?;
            }
        }
        let span = self
            .eat(TokenKind::RParen, "expected a closing `)` after a function call")?
            .span
            .merge(&lhs.span);
        Some(Expr {
            kind: ExprKind::Call { callee: box lhs, args },
            span,
        })
    }

    fn parse_index(&mut self, lhs: Expr) -> Option<Expr> {
        self.eat(TokenKind::LBracket, "expected `[` to index into data")?;
        let index = self.parse_expr()?;
        let span = self
            .eat(TokenKind::RBracket, "expected closing `]` after index operation")?
            .span
            .merge(&lhs.span);

        Some(Expr {
            kind: ExprKind::Index {
                callee: box lhs,
                index:  box index,
            },
            span,
        })
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        match self.peek_kind() {
            Some(TokenKind::Lit(lit)) => {
                let span = self.next_t()?.span;
                Some(Expr {
                    kind: ExprKind::Lit(lit),
                    span,
                })
            },
            Some(TokenKind::LParen) => self.parse_tup_expr(),
            Some(TokenKind::LBracket) => self.parse_array_init(),
            Some(TokenKind::Ident(_)) => {
                let path = self.parse_path()?;
                if let Some(TokenKind::LBrace) = self.peek_kind() {
                    return self.parse_struct_lit(path);
                }
                if path.len() == 1 {
                    let span = path.span;
                    let var = path.segments[0].clone();
                    Some(Expr {
                        kind: ExprKind::Var(var),
                        span,
                    })
                } else {
                    let span = path.span.clone();
                    // TODO(Simon): allow for expressions in enum variants
                    // example: Option::Some(42)
                    Some(Expr {
                        kind: ExprKind::Path(path),
                        span,
                    })
                }
            },
            Some(TokenKind::Keyword(Keyword::This)) => Some(Expr {
                kind: ExprKind::This,
                span: self.next_t()?.span,
            }),
            Some(TokenKind::LBrace) => {
                let block = self.parse_block()?;
                let span = block.span.clone();
                Some(Expr {
                    kind: ExprKind::Block(box block),
                    span,
                })
            },
            _ => {
                let span = self.next_t()?.span;
                self.sess.span_err("expected an expression", span);
                None
            },
        }
    }

    fn parse_ident<S: Into<String>>(&mut self, note: S) -> Option<Ident> {
        let t = self.next_t()?;
        if let TokenKind::Ident(data) = t.kind {
            Some(Ident { data, span: t.span })
        } else {
            self.sess
                .span_err(format!("expected identifier found: `{}`", t.kind), t.span)
                .add_note(note);
            None
        }
    }

    fn parse_tup_expr(&mut self) -> Option<Expr> {
        let start = self
            .eat(TokenKind::LParen, "expected `(` to start a tuple expression")?
            .span;

        let mut args = vec![];
        while Some(TokenKind::RParen) != self.peek_kind() {
            let expr = self.parse_expr()?;
            args.push(expr);

            if self.peek_kind() != Some(TokenKind::RParen) {
                self.eat(
                    TokenKind::Comma,
                    "expected a `,` to seperate the different elements of the tuple",
                )?;
            }
        }
        let end = self
            .eat(TokenKind::RParen, "expected a closing `)` after a tuple expression")?
            .span;
        Some(Expr {
            kind: ExprKind::Tup(args),
            span: start.merge(&end),
        })
    }

    fn parse_array_init(&mut self) -> Option<Expr> {
        let start = self
            .eat(TokenKind::LBracket, "expected a `[` to start an array init")?
            .span;
        let first = self.parse_expr()?;

        let arr = if self.peek_kind() == Some(TokenKind::Semi) {
            self.next_t()?;
            let len = self.parse_expr()?;

            ArrayInit::Filler {
                val: box first,
                len: box len,
            }
        } else {
            let mut init_list = vec![];

            while self.peek_kind() != Some(TokenKind::RBracket) {
                let expr = self.parse_expr()?;
                init_list.push(expr);

                if let Some(TokenKind::RBracket) = self.peek_kind() {
                    break;
                }
                self.eat(
                    TokenKind::Comma,
                    "expected `,` to seperate expressions in array initialization",
                )?;
            }

            ArrayInit::List(init_list)
        };

        let span = self
            .eat(TokenKind::RBracket, "expected closing `]` after array initialization")?
            .span
            .merge(&start);

        Some(Expr {
            kind: ExprKind::Array(arr),
            span,
        })
    }

    fn parse_struct_lit(&mut self, path: Path) -> Option<Expr> {
        self.eat(TokenKind::LBrace, "expected a `{` to start a struct literal")?;
        let mut fields = vec![];
        while self.peek_kind() != Some(TokenKind::RBrace) {
            let name = self.parse_ident("expected name of struct field to initialize")?;
            self.eat(
                TokenKind::Col,
                "the name of a struct field and the his value is seperated by a `:`",
            )?;
            let init = self.parse_expr()?;
            fields.push((name, init));
        }
        let span = self
            .eat(TokenKind::RBrace, "expected a closing `}` after a struct literal")?
            .span
            .merge(&path.first().unwrap().span);
        Some(Expr {
            kind: ExprKind::Struct { path, fields },
            span,
        })
    }

    fn parse_pattern(&mut self) -> Option<Pat> {
        let first = self.parse_single_pattern()?;

        if let Some(TokenKind::Sep) = self.peek_kind() {
            let mut pats = vec![first];
            while let Some(TokenKind::Sep) = self.peek_kind() {
                self.next_t()?;
                pats.push(self.parse_single_pattern()?);
            }
            // This can never fail because we bail out on the first pattern
            // So there is always going to be eat least one pattern in the vector
            let first = &pats.first().unwrap().span;
            let last = &pats.last().unwrap().span;
            let span = first.merge(last);
            Some(Pat {
                kind: PatKind::Or(pats),
                span,
            })
        } else {
            Some(first)
        }
    }

    fn parse_single_pattern(&mut self) -> Option<Pat> {
        if let Some(TokenKind::WildCard) = self.peek_kind() {
            return Some(Pat {
                kind: PatKind::WildCard,
                span: self.next_t()?.span,
            });
        }
        let expr = self.parse_expr()?;
        let span = expr.span.clone();
        Some(Pat {
            kind: PatKind::Expr(expr),
            span,
        })
    }

    fn parse_path(&mut self) -> Option<Path> {
        let first = self.parse_ident("expected path segemnt")?;
        let mut segments = vec![first];

        while let Some(TokenKind::ColCol) = self.peek_kind() {
            self.next_t()?;
            let segment = self.parse_ident("expected path segment")?;
            segments.push(segment);
        }

        // This can never fail because we bail out on the first path_ident
        // So there is always going to be eat least one segment in the vector
        let first = &segments.first().unwrap().span;
        let last = &segments.last().unwrap().span;
        let span = first.merge(last);
        Some(Path { segments, span })
    }

    fn peek_next_parsing_directive(&self) -> Option<NextDeclParseDirective> {
        let mut i = 0;
        loop {
            match self.peek_n_kind(i) {
                Some(TokenKind::Keyword(Keyword::Enum)) => return Some(NextDeclParseDirective::Enum),
                Some(TokenKind::Keyword(Keyword::Struct)) => return Some(NextDeclParseDirective::Struct),
                Some(TokenKind::Lit(_) | TokenKind::RBracket | TokenKind::Plus | TokenKind::Minus) => {
                    return Some(NextDeclParseDirective::ConstDecl)
                },
                Some(TokenKind::Col | TokenKind::LBrace | TokenKind::ThinArrow) => {
                    return Some(NextDeclParseDirective::Func)
                },
                Some(TokenKind::Hash) => return Some(NextDeclParseDirective::HashDirective),
                None => return None,
                _ => i += 1,
            };
        }
    }

    fn eat(&mut self, expected: TokenKind, note: &str) -> Option<Token> {
        match self.next_t() {
            Some(t) if t.kind == expected => Some(t),
            Some(t) => {
                let msg = format!(
                    "Expected the following token: `{}` at this point instead of `{}`",
                    expected, t.kind
                );
                self.sess.span_err(msg, t.span).add_note(note);
                None
            },
            None => None,
        }
    }

    fn peek(&self) -> Option<Token> { self.peek_n(1) }

    fn peek_n_kind(&self, n: usize) -> Option<TokenKind> { self.peek_n(n).map(|t| t.kind) }

    fn peek_kind(&self) -> Option<TokenKind> { self.peek().map(|t| t.kind) }

    fn peek_n(&self, n: usize) -> Option<Token> {
        let n = n.saturating_sub(1);
        self.buf.get(self.cursor + n).cloned()
    }

    fn next_t(&mut self) -> Option<Token> {
        // self.eat_whitespace()?;
        let cursor = self.cursor;
        self.cursor += 1;

        self.buf.get(cursor).cloned()
    }

    fn has_next(&self) -> bool { self.buf.len() > self.cursor }
}

#[derive(Debug, Copy, Clone)]
enum NextDeclParseDirective {
    Func,
    Enum,
    Struct,
    ConstDecl,
    HashDirective,
}
