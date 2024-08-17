/// OptimizationLevel flag for the CLI.
/// Implements `From<inkwell::OptimizationLevel> for OptimizationLevel`.
#[derive(clap::ValueEnum, Copy, Clone, Debug)]
pub(crate) enum OptimizationLevel {
    None,
    Less,
    Default,
    Aggressive,
}

impl From<OptimizationLevel> for inkwell::OptimizationLevel {
    fn from(value: OptimizationLevel) -> inkwell::OptimizationLevel {
        match value {
            OptimizationLevel::None => inkwell::OptimizationLevel::None,
            OptimizationLevel::Less => inkwell::OptimizationLevel::Less,
            OptimizationLevel::Default => inkwell::OptimizationLevel::Default,
            OptimizationLevel::Aggressive => inkwell::OptimizationLevel::Aggressive,
        }
    }
}

impl Default for OptimizationLevel {
    fn default() -> Self {
        // ;)
        Self::Aggressive
    }
}
