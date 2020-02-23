#![feature(type_ascription)]
#![feature(box_patterns)]
#![feature(box_syntax)]

use argh::FromArgs;
use std::sync::mpsc::*;
use std::fs;
use crate::types::error::Error;
use crate::parser::lex::Lexer;

mod parser;
mod types;

#[derive(FromArgs)]
/// phraxos language
struct CliArgs {
    #[argh(positional)]
    /// file name for the source file
    src_name: String
}

// stolen from https://github.com/jackmott/rust-lexer/
fn main() -> Result<(), Error> {
    let CliArgs { src_name }: CliArgs = argh::from_env();
    let input =
      fs::read_to_string(&src_name)?;
    rayon::scope(|s| {
        let (sender, receiver) = channel();
        s.spawn(|_| {
            Lexer::lex(&input, sender)
        });
        let mut r = receiver.recv();
        let mut res = Ok(());
        while let Ok(m) = r {
            match m {
                Ok(token) => {
                    println!("Token received from channel: {:?}", token);
                }
                Err(e) => {
                    println!("Error received from channel: {:?}", e);
                    res = Err(Error::LexerError(e))
                }
            }
            r = receiver.recv()
        }
        res
    })
}
