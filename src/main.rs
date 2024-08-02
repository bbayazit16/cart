use crate::lexer::Lexer;
use crate::reporter::Reporter;
use std::fs::File;
use std::io::BufReader;

mod error;
mod lexer;
mod reporter;
mod token;

fn main() {
    let file_path = "/Users/baris/dev/Languages/Rust/wasm-rust-compiler/program.rsty";
    let input_file = File::open(file_path).unwrap();

    let reader = BufReader::new(input_file);
    let mut lexer = Lexer::new(reader, Reporter::new(file_path.into()));

    while !lexer.is_at_end() {
        if let Ok(token) = lexer.request_next_token() {
            println!("{}", token)
        } else {
            std::process::exit(1);
        }
        // lexer.request_next_token();
    }
}
