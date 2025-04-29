use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use crate::codegen::value::Value;
use crate::codegen::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for r-value variable expressions.
    pub(super) fn generate_variable_r_value(&mut self, name: &str) -> Value<'ctx> {
        // As verified by the type checker, at this point, the variable must exist.
        // Get the alloca instruction for the variable from the symbol table.
        // This points to the innermost scope where the variable is defined.
        let variable_value = *self.symbol_table.get(name).unwrap();
        let variable_r_value = self.builder.build_load(
            BasicTypeEnum::from(variable_value),
            BasicValueEnum::from(variable_value).into_pointer_value(),
        &format!("rv_cast_{}", name)
        ).unwrap();

        Value::new(BasicTypeEnum::from(variable_value), variable_r_value)
    }
    
    /// Generates LLVM IR for l-value variable expressions.
    pub(super) fn generate_variable_l_value(&mut self, name: &str) -> Value<'ctx> {
        // Same method as generate_variable_r_value.
        *self.symbol_table.get(name).unwrap()
    }
}
