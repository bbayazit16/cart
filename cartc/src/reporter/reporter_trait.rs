use crate::errors::CompileError;
use std::path::Path;

pub trait Reporter {
    /// Create a new instance of the reporter.
    fn new<P: AsRef<Path>>(file_path: P) -> Self;

    /// Report a `CompileError`.
    fn report(&self, error: &CompileError);
}
