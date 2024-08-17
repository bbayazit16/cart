//! Builds the command line interface for the compiler via clap.

mod opt_level;
pub(crate) use opt_level::OptimizationLevel;

use std::path::PathBuf;

#[derive(clap::Parser, Debug)]
#[command(name = "cart", about = "Just a compiler.")]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(clap::Args, Debug)]
pub(crate) struct CommonOptions {
    #[arg(value_name = "FILE")]
    pub input: PathBuf,

    #[arg(short, long, value_name = "OUTPUT")]
    pub output: Option<String>,

    #[arg(long, default_value = "main", value_name = "ENTRYPOINT")]
    pub entrypoint: String,

    #[arg(long)]
    pub emit_ir: bool,

    #[arg(long, default_value = "aggressive", value_enum)]
    pub optimization: OptimizationLevel,
}

#[derive(clap::Subcommand, Debug)]
pub(crate) enum Commands {
    Compile(CommonOptions),
    Run(CommonOptions),
}
