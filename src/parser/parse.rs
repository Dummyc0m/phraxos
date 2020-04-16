use std::sync::mpsc::*;
use std::convert::TryInto;
use crate::parser::lex::{Token, TokenType};
use crate::types::ast;
use crate::types::ast::*;

use crate::parser::pres_ctx::{PresCtx, BindingPower};

#[derive(PartialEq, Eq, Debug)]
enum TokenLookahead<'a> {
    Nothing,
    Current(Token<'a>),
    CurrentAndNext(Token<'a>, Token<'a>)
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct ParseError<'a> {
    pub expected: &'static str,
    pub actual: Option<Token<'a>>,
}

impl <'a> ParseError<'a> {
    fn empty(expected: &'static str) -> ParseError<'a> {
        ParseError {
            expected,
            actual: None
        }
    }

    fn new(expected: &'static str, actual: Token<'a>) -> ParseError<'a> {
        ParseError {
            expected,
            actual: Some(actual)
        }
    }
}

pub struct Parser<'a> {
    raw_str: &'a str,
    input: Receiver<Token<'a>>,
    token: TokenLookahead<'a>,
}

impl <'a> Parser<'a> {
    pub fn parse(raw_str: &'a str, input: Receiver<Token<'a>>) -> Result<Prog, ParseError<'a>> {
        let mut parser = Self::new(raw_str, input);
        let decls = parser.decls(&mut PresCtx::new(), |t| t == TokenType::Eof)?;
        Ok(Prog(decls))
    }

    fn new(raw_str: &'a str, input: Receiver<Token<'a>>) -> Self {
        Parser {
            raw_str, input,
            token: TokenLookahead::Nothing,
        }
    }

    fn decl(&mut self, pres_ctx: &mut PresCtx) -> Result<Decl, ParseError<'a>> {
        use crate::parser::lex::TokenType::*;
        match self.eat("Decl")? {
            Token(Let, _) => {
                let var = self.ident()?;
                self.skip(Equal)?;
                let expr = self.expr(pres_ctx)?;
                Ok(Decl::Vbind(var, expr))
            }
            Token(Type, _) => {
                let var = self.ident()?;
                self.skip(Equal)?;
                let tycon = self.tycon()?;
                Ok(Decl::Type(var, tycon))
            }
            Token(Fun, _) => {
                Err(ParseError::empty("fun not supported"))
            }
            d @ Token(Infix, _) => {
                let left: u8 = self.number()?.try_into().map_err(|_| ParseError::new("small natural number", d))?;
                let right: u8 = self.number()?.try_into().map_err(|_| ParseError::new("small natural number", d))?;

                let operator = self.ident()?;
                pres_ctx.insert(operator.clone(), BindingPower(left, right));
                Ok(Decl::Infix(left, right, operator))
            }
            actual => Err(ParseError::new("Decl", actual))
        }
    }

    fn skip(&mut self, lit: TokenType<'a>) -> Result<(), ParseError<'a>> {
        // TODO give better hint
        let ate = self.eat("literal")?;
        if lit == ate.typ() {
            Ok(())
        } else {
            Err(ParseError::new("literal", ate))
        }
    }

    fn ident(&mut self) -> Result<Ident, ParseError<'a>> {
        use crate::parser::lex::TokenType::*;
        match self.eat("Ident")? {
            Token(Ident(s), _) => Ok(ast::Ident(s.to_owned())),
            actual => Err(ParseError::new("Ident", actual))
        }
    }

    fn number(&mut self) -> Result<i32, ParseError<'a>> {
        use crate::parser::lex::TokenType::*;
        match self.eat("Number")? {
            Token(Number(n), _) => Ok(n),
            actual => Err(ParseError::new("Number", actual))
        }
    }

    fn decls<P: Fn(TokenType<'a>) -> bool>(&mut self, pres_ctx: &mut PresCtx, until: P) -> Result<Vec<Decl>, ParseError<'a>> {
        let mut decls = Vec::new();
        while !until(self.curr("Decl")?.typ()) {
            decls.push(self.decl(pres_ctx)?)
        }
        Ok(decls)
    }

    fn tycon(&mut self) -> Result<Tycon, ParseError<'a>> {
        use crate::parser::lex::TokenType::*;
        let mut lhs = match self.eat("Tycon")? {
            Token(Ident(s), _) => Tycon::Ident(ast::Ident(s.to_owned())),
            Token(LParen, _) => {
                let content = self.tycon()?;
                self.skip(RParen)?;
                content
            }
            actual => return Err(ParseError::new("Tycon", actual))
        };

        while let Token(Arrow, _) = self.curr("Tycon")? {
            self.skip(Arrow)?;
            let rhs = self.tycon()?;
            lhs = Tycon::Arrow(box lhs, box rhs)
        }

        Ok(lhs)
    }

    fn expr(&mut self, pres_ctx: &PresCtx) -> Result<Expr, ParseError<'a>> {
        self.expr_bp(pres_ctx, 0)
    }

    fn expr_bp(&mut self, pres_ctx: &PresCtx, min_bp: u8) -> Result<Expr, ParseError<'a>> {
        use crate::parser::lex::TokenType::*;
        let mut lhs = match self.curr("Expr")? {
            Token(Extern, _) => {
                self.skip(Extern)?;
                return Ok(Expr::Extern)
            }
            Token(Ident(_), _) => {
                Expr::Var(self.ident()?)
            },
            Token(Number(n), _) => { self.eat("Number")?; Expr::Number(n) },

            Token(If, _) => {
                self.eat("If")?;
                let condition = self.expr(&pres_ctx)?;
                self.skip(Then)?;
                let then_branch = self.expr(&pres_ctx)?;
                self.skip(Else)?;
                let else_branch = self.expr(&pres_ctx)?;
                Expr::If(box condition, box then_branch, box else_branch)
            }

            Token(Let, _) |
            Token(Type, _) |
            Token(Fun, _) |
            Token(Infix, _) => {
                let mut ctx = pres_ctx.clone();
                let decls = self.decls(&mut ctx, |t| t == In)?;
                self.skip(In)?;
                let body = self.expr(&ctx)?;
                Expr::Let(decls, box body)
            }

            Token(LParen, _) => {
                self.skip(LParen)?;
                let content = self.expr(pres_ctx)?;
                self.skip(RParen)?;
                content
            }

            Token(Backslash, _) => {
                self.skip(Backslash)?;
                let var = self.ident()?;
                self.skip(Colon)?;
                let tycon = self.tycon()?;
                self.skip(Dot)?;
                let body = self.expr(pres_ctx)?;
                Expr::Lam(var, tycon, box body)
            }

            actual => return Err(ParseError::new("Expr", actual)),
        };

        loop {
            // doesn't actually eat until it's sure
            let (l_bp, r_bp) = match self.curr("Op")? {
                Token(LParen, _) => {
                    self.skip(LParen)?;
                    let content = self.expr(pres_ctx)?;
                    self.skip(RParen)?;
                    lhs = Expr::Ap(box lhs, box content);
                    continue
                }

                Token(Ident(s), _) => {
                    match pres_ctx.get(&ast::Ident(s.to_owned())) {
                        None => {
                            let ident = self.ident()?;
                            lhs = Expr::Ap(box lhs, box Expr::Var(ident));
                            continue
                        }
                        Some(BindingPower(lbp, rbp)) => (*lbp, *rbp)
                    }
                }

                Token(Dot, _) => {
                    match self.peek("Ident")? {
                        Token(Ident(_), _) => {
                            self.eat("Dot")?;
                            let ident = self.ident()?;
                            lhs = Expr::Dot(box lhs, ident);
                            continue
                        }
                        _ => {
                            let ident = ast::Ident(".".to_owned());
                            match pres_ctx.get(&ident) {
                                None => {
                                    self.eat("Dot")?;
                                    lhs = Expr::Ap(box lhs, box Expr::Var(ident));
                                    continue
                                }
                                Some(BindingPower(l_bp, r_bp)) => (*l_bp, *r_bp)
                            }
                        }
                    }
                }

                Token(Number(n), _) => {
                    self.eat("Number")?;
                    lhs = Expr::Ap(box lhs, box Expr::Number(n));
                    continue
                }

                Token(Where, _) => {
                    return Err(ParseError::empty("where not supported"))?
                }

                _ => break
            };

            if l_bp < min_bp {
                break
            }

            let ident = self.ident()?;
            let rhs = self.expr_bp(&pres_ctx, r_bp)?;
            lhs = Expr::BinOp(box lhs, ident, box rhs)
        }

        Ok(lhs)
    }


    fn receive_token(&mut self) -> Option<Token<'a>> {
        self.input.recv().map_or_else(|_| None, |t| Some(t))
    }

    fn eat(&mut self, expected: &'static str) -> Result<Token<'a>, ParseError<'a>> {
        match self.token {
            TokenLookahead::Nothing => {
                self.receive_token().ok_or_else(|| ParseError::empty(expected))
            }
            TokenLookahead::Current(t) => {
                self.token = TokenLookahead::Nothing;
                Ok(t)
            },
            TokenLookahead::CurrentAndNext(t, n) => {
                self.token = TokenLookahead::Current(n);
                Ok(t)
            },
        }
    }

    fn curr(&mut self, expected: &'static str) -> Result<Token<'a>, ParseError<'a>> {
        match self.token {
            TokenLookahead::Nothing => {
                self.receive_token().map(|t| {
                    self.token = TokenLookahead::Current(t);
                    t
                }).ok_or_else(|| ParseError::empty(expected))
            }
            TokenLookahead::Current(t) => Ok(t),
            TokenLookahead::CurrentAndNext(t, _) => Ok(t),
        }
    }

    fn peek(&mut self, expected: &'static str) -> Result<Token<'a>, ParseError<'a>> {
        match self.token {
            TokenLookahead::Nothing => {
                self.receive_token().and_then(|t1| {
                    self.token = TokenLookahead::Current(t1);
                    self.receive_token().map(|t2| {
                        self.token = TokenLookahead::CurrentAndNext(t1, t2);
                        t2
                    })
                }).ok_or_else(|| ParseError::empty(expected))
            }
            TokenLookahead::Current(t1) => {
                self.receive_token().map(|t2| {
                    self.token = TokenLookahead::CurrentAndNext(t1, t2);
                    t2
                }).ok_or_else(|| ParseError::empty(expected))
            }
            TokenLookahead::CurrentAndNext(_, t2) => Ok(t2)
        }
    }
}
