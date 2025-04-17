use crate::codegen::value::{LValue, Value};
use crate::codegen::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for variable expressions.
    pub(super) fn generate_variable(&mut self, name: &str) -> Value<'ctx, LValue> {
        // As verified by the type checker, at this point, the variable must exist.
        // Get the alloca instruction for the variable from the symbol table.
        // This points to the innermost scope where the variable is defined.
        *self.symbol_table.get(name).unwrap()
    }
}
