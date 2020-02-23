use crate::parser::lex::LexerError;

#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
    TypeError,
    LexerError(LexerError)
}

impl From<std::io::Error> for Error {
    fn from(other: std::io::Error) -> Self {
        Error::IOError(other)
    }
}
