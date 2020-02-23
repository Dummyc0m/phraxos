use std::sync::mpsc::*;
use std::char;
use std::str::CharIndices;
use std::iter::Peekable;
use std::num::ParseIntError;
use unicode_width::UnicodeWidthChar;
use unicode_xid::UnicodeXID;

#[derive(Eq, PartialEq, Debug)]
pub enum LexerError {
    Match(TokenPos),
    Comment(TokenPos),
    LargeInt(TokenPos),
    Empty(TokenPos),
    Hex(Option<ParseIntError>, TokenPos),
    Int(Option<ParseIntError>, TokenPos),
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
    Underscore,
    Backslash,

    Plus,
    Minus,
    Times,
    Div,
    Mod,

    Let,
    In,
    Int,

    Eof,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Token<'a>(pub TokenType<'a>, pub TokenPos);

pub type LexerResult<'a> = Result<Token<'a>, LexerError>;

#[derive(Eq, PartialEq, Debug)]
pub struct TokenPos {
    pub line: usize,
    pub column: usize,
}

struct LexerState<'a>(fn (&mut Lexer<'a>) -> Option<LexerState<'a>>);

pub struct Lexer<'a> {
    input: Peekable<CharIndices<'a>>,
    raw_str: &'a str,
    // position relative to raw_str
    pos: usize,
    line: usize,
    // unicode position within the current line
    column: usize,
    sender: Sender<LexerResult<'a>>,
}

impl <'a> Lexer<'a> {
    pub fn lex(input: &'a str, sender: Sender<LexerResult<'a>>) {
        let mut lexer = Lexer::new(input, sender);
        lexer.run()
    }

    fn new(input: &'a str, sender: Sender<LexerResult<'a>>) -> Self{
        Lexer {
            input: input.char_indices().peekable(),
            raw_str: input,
            pos: 0,
            line: 0,
            column: 0,
            sender
        }
    }

    fn pos(&self) -> TokenPos {
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

    fn current(&mut self) -> Option<char> {
        self.input.peek().map(|&(_, c)| c)
    }

    fn single(&mut self, token: TokenType<'a>) -> Option<LexerState<'a>> {
        self.emit(token);
        self.skip();
        Some(LexerState(Self::tokenize))
    }

    fn skip(&mut self) {
        self.input.next().and_then(|(_, c)| {
            self.pos = self.input.peek().map(|&(i, _)| i)?;
            if Self::is_newline(c) {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += c.width().unwrap_or(0);
            }
            Some(())
        });
    }

    fn skip_while<P: Fn(char) -> bool>(&mut self, pred: P) {
        let mut curr = match self.current() {
            None => return,
            Some(v) => v
        };
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
            Some(&self.raw_str[start_pos..self.pos])
        }
    }

    fn skip_whitespaces(&mut self) -> Option<LexerState<'a>> {
        self.skip_while(|c| c.is_whitespace());
        Some(LexerState(Self::tokenize))
    }

    fn line_comment(&mut self) -> Option<LexerState<'a>> {
        self.skip_while(|c| !Self::is_newline(c));
        self.skip();
        Some(LexerState(Self::tokenize))
    }

    fn block_comment(&mut self) -> Option<LexerState<'a>> {
        self.skip();
        let mut level = 1;
        while level > 0 {
            self.skip_while(|c| c != '*' && c != '/');
            let prev = self.current();
            self.skip();
            match (prev, self.current()) {
                (Some(prev), Some(curr)) => {
                    self.skip();
                    match (prev, curr) {
                        ('*', '/') => {
                            level -= 1;
                        }
                        ('/', '*') => {
                            level += 1;
                        }
                        (..) => ()
                    }
                }
                _ => {
                    self.err(LexerError::Comment(self.pos()));
                    return None
                }
            }
        }
        Some(LexerState(Self::tokenize))
    }

    fn hex(&mut self) -> Option<LexerState<'a>> {
        fn leading_zeros(buf: &str) -> usize {
            for (i, chr) in buf.char_indices() {
                if chr != '0' {
                    return i;
                }
            }
            buf.len()
        }
        self.skip();
        match self.current() {
            Some('X') | Some('x') => {
                self.skip();
                let hex_value = match self.take_while(|c| c.is_ascii_hexdigit()) {
                    None => {
                        self.err(LexerError::Hex(None, self.pos()));
                        return None
                    }
                    Some(v) => v,
                };

                if hex_value.len() - leading_zeros(hex_value) > 8 {
                    self.err(LexerError::LargeInt(self.pos()));
                    None
                } else {
                    let result = match i64::from_str_radix(&hex_value, 16) {
                        Err(e) => {
                            self.err(LexerError::Hex(Some(e), self.pos()));
                            return None
                        }
                        Ok(v) => v,
                    };
                    self.emit(TokenType::Number(result as i32));
                    Some(LexerState(Self::tokenize))
                }
            }
            _ => {
                self.emit(TokenType::Number(0));
                Some(LexerState(Self::tokenize))
            }
        }
    }

    fn dec(&mut self) -> Option<LexerState<'a>> {
        static INT_MIN: i64 = -2_147_483_648;
        static INT_MAX: i64 = 2_147_483_648;

        let dec_val = match self.take_while(|c| c.is_numeric()) {
            None => {
                self.err(LexerError::Int(None, self.pos()));
                return None
            }
            Some(v) => v,
        };

        let result: i64 = match dec_val.parse() {
            Err(e) => {
                self.err(LexerError::Int(Some(e), self.pos()));
                return None
            }
            Ok(v) => v,
        };
        if result >= INT_MIN && result <= INT_MAX {
            self.emit(TokenType::Number(result as i32));
            Some(LexerState(Self::tokenize))
        } else {
            self.err(LexerError::LargeInt(self.pos()));
            None
        }
    }

    fn ident(&mut self) -> Option<LexerState<'a>> {
        let ident_val = match self.take_while(|c| c.is_xid_continue()) {
            None => {
                self.err(LexerError::Empty(self.pos()));
                return None
            }
            Some(v) => v,
        };
        match ident_val {
            // keywords
            "let" => self.emit(TokenType::Let),
            "in" => self.emit(TokenType::In),
            "int" => self.emit(TokenType::Int),
            "_" => self.emit(TokenType::Underscore),
            _ => self.emit(TokenType::Ident(ident_val)),
        }
        Some(LexerState(Self::tokenize))
    }

    fn slash(&mut self) -> Option<LexerState<'a>> {
        self.skip();
        match self.current() {
            Some('/') => Some(LexerState(Self::line_comment)),
            Some('*') => Some(LexerState(Self::block_comment)),
            _ => {
                self.emit(TokenType::Div);
                Some(LexerState(Self::tokenize))
            }
        }
    }

    fn minus(&mut self) -> Option<LexerState<'a>> {
        self.skip();
        match self.current() {
            Some('>') => {
                self.skip();
                self.emit(TokenType::Arrow);
                Some(LexerState(Self::tokenize))
            }
            _ => {
                self.emit(TokenType::Minus);
                Some(LexerState(Self::tokenize))
            }
        }
    }

    fn tokenize(self: &mut Lexer<'a>) -> Option<LexerState<'a>> {
        self.current().and_then(|c| {
            if c.is_whitespace() {
                Some(LexerState(Self::skip_whitespaces))
            } else {
                match c {
                    '(' => self.single(TokenType::LParen),
                    ')' => self.single(TokenType::RParen),
                    ':' => self.single(TokenType::Colon),
                    '=' => self.single(TokenType::Equal),
                    '\\' => self.single(TokenType::Backslash),
                    '/' => Some(LexerState(Self::slash)),
                    '%' => self.single(TokenType::Mod),
                    '*' => self.single(TokenType::Times),
                    '+' => self.single(TokenType::Plus),
                    '-' => Some(LexerState(Self::minus)),
                    '0' => Some(LexerState(Self::hex)),
                    '1'..='9' => Some(LexerState(Self::dec)),

                    _ => {
                        if c.is_xid_start() || c == '_' {
                            Some(LexerState(Self::ident))
                        } else {
                            self.err(LexerError::Match(self.pos()));
                            None
                        }
                    }
                }
            }
        })
    }

    // trampoline style
    fn run(&mut self) {
        let mut next = Some(LexerState(Lexer::tokenize));
        while let Some(LexerState(n)) = next {
            next = n(self)
        }
        self.emit(TokenType::Eof)
    }

    fn is_newline(c: char) -> bool {
        c == '\n'
    }
}
