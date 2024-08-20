//! Builds the command line interface for the compiler via clap.

mod opt_level;
pub(crate) use opt_level::OptimizationLevel;

use std::path::PathBuf;

#[derive(clap::Parser, Debug)]
#[command(name = "cart", about = "The Cart compiler.")]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(clap::Args, Debug)]
pub(crate) struct CommonOptions {
    #[arg(value_name = "FILE", help = "The file to compile")]
    pub input: PathBuf,

    #[arg(short, long, value_name = "OUTPUT", help = "The output file name")]
    pub output: Option<String>,

    #[arg(long, help = "Time compilation")]
    pub time_compilation: bool,

    #[arg(
        long,
        default_value = "main",
        value_name = "ENTRYPOINT",
        help = "The entrypoint function"
    )]
    pub entrypoint: String,

    #[arg(long, help = "Emit LLVM IR to console after compilation")]
    pub emit_ir: bool,

    #[arg(
        long,
        default_value = "aggressive",
        value_enum,
        help = "Optimization level"
    )]
    pub optimization: OptimizationLevel,
}

#[derive(clap::Subcommand, Debug)]
pub(crate) enum Commands {
    #[command(about = "Compile a file")]
    Compile(CommonOptions),
    #[command(about = "Compile and run a file")]
    Run(CommonOptions),
}
