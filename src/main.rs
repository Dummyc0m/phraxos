#![feature(type_ascription)]
#![feature(box_patterns)]
#![feature(box_syntax)]

use argh::FromArgs;
use std::sync::mpsc::*;
use crate::types::error::Error;

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
      fs::read_to_string(&filename)?;
    rayon::scope(|s| {
        let (send, receive) = channel();
        s.spawn(|_| {
            Lexer::lex(&input, send)
        });
        while let Ok(token) = receiver.recv() {
            println!("Token received from channel: {:?}", token);
        }
    })
}
