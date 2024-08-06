mod lexer;
mod token;

use crate::context::FileContext;
use crate::parser::Parser;

mod ast;
mod context;
mod errors;
mod parser;
mod reporter;

#[allow(unused_macros)]
macro_rules! lexer_debug {
    ($context:ident) => {
        let mut lexer = crate::lexer::Lexer::new($context);
        while !lexer.is_at_end() {
            dbg!(&lexer.request_next_token().unwrap());
        }
        std::process::exit(0);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file_path = "/Users/baris/dev/Languages/Rust/wasm-rust-compiler/program.rsty";
    let context = FileContext::try_new(&file_path).unwrap();
    
    // lexer_debug!(context);

    let program = Parser::new(context).parse();
    if let Ok(program) = program {
        println!("{}", program);
    }

    Ok(())
}
