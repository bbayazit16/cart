mod ast;
mod cli;
mod codegen;
mod context;
mod errors;
mod hir;
mod lexer;
mod parser;
mod reporter;
mod token;

use crate::cli::{Cli, Commands};
use crate::codegen::compiler::compile_and_or_run;
use crate::context::FileContext;
use crate::parser::Parser;
use crate::reporter::ConsoleReporter;
use clap::Parser as ClapParser;

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

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run(options) | Commands::Compile(options) => {
            let reporter = ConsoleReporter::new(&options.input);
            let context = FileContext::<ConsoleReporter>::try_new(
                &options.input, &reporter
            ).unwrap();

            let start = std::time::Instant::now();
            let program = Parser::new(context).parse();
            let end = std::time::Instant::now();

            if options.time_compilation {
                println!(
                    "Parsing complete in {}µ",
                    end.duration_since(start).as_micros()
                );
            }
            
            let mut hir = hir::TypeChecker::new(&reporter).resolve_types(&program);

            let start = std::time::Instant::now();
            compile_and_or_run(&mut hir, matches!(cli.command, Commands::Run(_)), options);
            let end = std::time::Instant::now();

            if options.time_compilation {
                println!(
                    "Compilation/running complete in {}ms",
                    end.duration_since(start).as_millis()
                );
            }
        }
    }
}
