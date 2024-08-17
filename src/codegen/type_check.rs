use crate::ast::Type;
use crate::errors::CompileError;
use crate::reporter::Reporter;
use std::collections::HashMap;

/// TypeChecker struct resolves variable types, generics, and scoped variables,
/// and assigns them to the AST nodes. If an error occurs, the TypeChecker
/// reports to the reporter.
#[allow(unused)]
pub(crate) struct TypeChecker {
    reporter: Reporter,
    errors: Vec<CompileError>,
    variable_map: Vec<HashMap<String, Type>>,
}
