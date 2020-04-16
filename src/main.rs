#![feature(type_ascription)]
#![feature(box_patterns)]
#![feature(box_syntax)]

use argh::FromArgs;
use std::sync::mpsc::*;
use std::fs;
use crate::types::error::Error;
use crate::parser::lex::Lexer;
use crate::parser::parse::Parser;

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
        let (sender, lex_receiver) = channel();
        s.spawn(|_| {
            Lexer::lex(&input, sender)
        });
        let (sender, parse_receiver) = channel();
        s.spawn(move |_| {
            let mut r = lex_receiver.recv();
            while let Ok(m) = r {
                match m {
                    Ok(token) => {
                        sender.send(token).unwrap_or_else(|e| println!("Lexer -> Parser: failed on {:?} with {:?}", token, e))
                    }
                    Err(e) => {
                        println!("Lexer: {:?}", e);
                        break
                    }
                }
                r = lex_receiver.recv()
            }
        });

        match Parser::parse(&input, parse_receiver) {
            Ok(prog) => {
                println!("{:#?}", prog)
            }
            Err(p) => println!("Parser: {:?}", p),
        }

        // let mut r = receiver.recv();
        // let mut res = Ok(());
        // while let Ok(m) = r {
        //     match m {
        //         Ok(token) => {
        //             println!("Token received from channel: {:?}", token);
        //         }
        //         Err(e) => {
        //             println!("Error received from channel: {:?}", e);
        //             res = Err(Error::LexerError(e))
        //         }
        //     }
        //     r = receiver.recv()
        // }
        // TODO FIXME
        Ok(())
    })
}
