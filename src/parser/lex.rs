use std::sync::mpsc::*;
use std::marker::PhantomData;
use std::char;
use std::str::Chars;
use std::iter::Peekable;

#[derive(Eq, PartialEq, Debug)]
pub enum LexerError {
    Match(TokenPos),
    LargeInt(TokenPos),
    Empty(TokenPos),
}

#[derive(Eq, PartialEq, Debug)]
pub enum TokenType<'a> {
    Ident(&'a str),
    Number(i32),

    LParen,
    RParen,
    Equal,
    Arrow,
    Colon,
    Backslash,

    Plus,
    Minus,
    Times,
    Div,
    Mod,

    Let,
    In,
    Int,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Token<'a>(pub TokenType<'a>, pub TokenPos);

pub type LexerResult<'a> = Result<Token<'a>, LexerError>;

#[derive(Eq, PartialEq, Debug)]
pub struct TokenPos {
    pub line: usize,
    pub column: usize,
}

struct LexerState(fn (&mut Lexer) -> Option<LexerState>);

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    raw_str: &'a str,
    pos: usize,
    line: usize,
    column: usize,
    sender: Sender<LexerResult<'a>>,
}

impl <'a> Lexer<'a> {
    pub fn lex(input: &'a str, sender: Sender<LexerResult<'a>>) {
        let mut lexer = Lexer::new();
        lexer.run()
    }

    fn new(input: &'a str, sender: Sender<LexerResult<'a>>) -> Self{
        Lexer {
            input: input.chars().peekable(),
            raw_str: input,
            pos: 0,
            line: 0,
            column: 0,
            sender
        }
    }

    fn pos(&self) {
        TokenPos {
            line: self.line,
            column: self.column,
        }
    }

    fn emit(&mut self, token: TokenType<'a>) {
        self.sender.send(Ok(Token(token, self.pos())))
                   .unwrap_or_else(|e| panic!("failed to send token {:?}", e))
    }

    fn err(&mut self, err: LexerError) {
        self.sender.send(Err(err))
                   .unwrap_or_else(|e| panic!("failed to send token {:?}", e))
    }

    fn current(&self) -> Option<char> {
        self.input.peek()
    }

    fn single(&mut self, token: TokenType<'a>) -> Option<LexerState> {
        self.emit(token);
        self.skip();
        Some(LexerState(Self::tokenize))
    }

    fn skip(&mut self) {
        self.input.next().map(|s| {
            self.pos += 1;
            if Self::is_newline(s) {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        })
    }

    fn skip_while<P: Fn(char) -> bool>(&mut self, pred: P) {
        let mut curr = self.current()?;
        while pred(curr) {
            self.skip();
            if let Some(c) = self.current() {
                curr = c
            } else {
                break;
            }
        }
    }

    fn take_while<P: Fn(char) -> bool>(&mut self, pred: P) -> Option<&'a str> {
        let mut curr = self.current()?;
        let start_pos = self.pos;
        while pred(curr) {
            self.skip();
            if let Some(c) = self.current() {
                curr = c
            } else {
                break;
            }
        }
        if start_pos == self.pos {
            // has not advanced
            self.err(LexerError::Empty(self.pos()));
            None
        } else {
            Some(self.raw_str[start_pos..self.pos])
        }
    }

    fn skip_whitespaces(&mut self) -> Option<LexerState> {
        self.skip_while(|c| c.is_whitespace());
        Some(LexerState(Self::tokenize))
    }

    fn run(&mut self) {

    }

    fn tokenize(&mut self) -> Option<LexerState> {
        self.current().map(|c| {
            if c.is_whitespace() {
                self.skip_whitespaces()
            } else {
                match c {
                    '(' => self.single(TokenType::LParen),
                    ')' => self.single(TokenType::RParen),
                    ':' => self.single(TokenType::Colon),
                    '=' => self.single(TokenType::Equal),
                    '\\' => self.single(TokenType::Backslash),
                    _ => {
                        self.err(LexerError::Match(self.pos()));
                        None
                    }
                }
            }
        })
    }

    fn is_newline(c: char) -> bool {
        c == '\n'
    }
}
