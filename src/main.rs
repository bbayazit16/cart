use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::reporter::Reporter;
use std::fs::File;
use std::io::BufReader;

mod ast;
mod error;
mod lexer;
mod parser;
mod reporter;
mod token;

// TODO: Multiline comments
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file_path = "/Users/baris/dev/Languages/Rust/wasm-rust-compiler/program.rsty";
    let input_file = File::open(file_path).unwrap();

    let reader = BufReader::new(input_file);
    let lexer = Lexer::new(reader, Reporter::new(file_path.into()));

    // while !lexer.is_at_end() {
    //     dbg!(&lexer.request_next_token().unwrap());
    // }
    // panic!();

    let mut parser = Parser::new(lexer, Reporter::new(file_path.into()));
    
    let program = parser.parse()?;
    dbg!(program);
    
    Ok(())

    // while !lexer.is_at_end() {
    //     if let Ok(token) = lexer.request_next_token() {
    //         println!("{}", token)
    //     } else {
    //         std::process::exit(1);
    //     }
    // }
}
