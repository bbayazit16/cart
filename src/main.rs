mod lexer;
mod token;

use crate::codegen::compile::compile;
use crate::context::FileContext;
use crate::parser::Parser;
use std::env;

mod ast;
mod codegen;
mod context;
mod errors;
mod parser;
mod reporter;

#[allow(unused_macros)]
macro_rules! lexer_debug {
    ($context:ident) => {
        let mut lexer = crate::lexer::Lexer::new($context);
        // let first_token = lexer.request_next_token();
        // dbg!(&first_token);
        while !lexer.is_at_end() {
            let a = lexer.request_next_token();
            dbg!(&a);
        }
        // lexer.revert_to_position(*first_token.get_file_pointer());
        // while !lexer.is_at_end() {
        //     let a = &lexer.request_next_token().unwrap();
        //     // dbg!(&a);
        // }
        std::process::exit(0);
    };
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let context = FileContext::try_new(file_path).unwrap();

    let start = std::time::Instant::now();
    let mut program = Parser::new(context).parse();
    // dbg!(&program);
    let end = std::time::Instant::now();
    println!(
        "Parsing complete in {}µ",
        end.duration_since(start).as_micros()
    );

    let ctx = inkwell::context::Context::create();
    let start = std::time::Instant::now();
    let main_fn = compile(&mut program, &ctx);
    let end = std::time::Instant::now();
    println!(
        "Compilation complete in {}ms",
        end.duration_since(start).as_millis()
    );

    let start = std::time::Instant::now();
    let result = main_fn();
    let end = std::time::Instant::now();
    println!(
        "Process finished with exit code {} in {}µ",
        result,
        end.duration_since(start).as_micros()
    );

    Ok(())
}
