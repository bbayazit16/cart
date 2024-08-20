mod lexer;
mod token;

use crate::cli::{Cli, Commands};
use crate::codegen::compiler::compile;
use crate::context::FileContext;
use crate::parser::Parser;
use clap::Parser as ClapParser;

mod ast;
mod cli;
mod codegen;
mod context;
mod errors;
mod parser;
mod reporter;

#[allow(unused_macros)]
macro_rules! lexer_debug {
    ($context:ident) => {
        let mut lexer = crate::lexer::Lexer::new($context);

        while !lexer.is_at_end() {
            let a = lexer.request_next_token();
            dbg!(&a);
        }

        std::process::exit(0);
    };
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run(options) | Commands::Compile(options) => {
            let context = FileContext::try_new(&options.input).unwrap();

            let start = std::time::Instant::now();
            let mut program = Parser::new(context).parse();
            let end = std::time::Instant::now();
            
            if options.time_compilation {
                println!(
                    "Parsing complete in {}µ",
                    end.duration_since(start).as_micros()
                );
            }

            let start = std::time::Instant::now();
            compile(
                &mut program,
                matches!(cli.command, Commands::Run(_)),
                options,
            );
            let end = std::time::Instant::now();

            if options.time_compilation {
                println!(
                    "Compilation complete in {}ms",
                    end.duration_since(start).as_millis()
                );
            }
        }
    }

    Ok(())
}
