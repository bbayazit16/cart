mod functions;
mod structs;

use crate::codegen::CodeGen;
use crate::hir::Declaration;

impl CodeGen<'_> {
    /// Generates the LLVM IR for a declaration.
    pub(super) fn generate_declaration(&mut self, declaration: &Declaration) {
        match declaration {
            Declaration::Function(ref function) => self.generate_function(function),
            Declaration::Statement(ref stmt) => self.generate_statement(stmt),
            Declaration::Struct {
                ref name,
                ref fields,
                .. // generic_declarations are unused
            } => self.generate_struct(name, fields),
            Declaration::Extension { functions, .. } => {
                for function in functions {
                    self.generate_function(function);
                }
            }
        };
    }
}
