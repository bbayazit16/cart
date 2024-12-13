use crate::errors::CompileError;
use std::path::Path;

pub trait Reporter {
    /// Report a `CompileError`.
    fn report(&self, error: &CompileError);
}
