use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::Type;
use inkwell::values::BasicValue;

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for variable expressions.
    pub(super) fn generate_variable(&mut self, name: &str, ty: &Type) -> Value<'ctx> {
        if let Some(function) = self.module.get_function(name) {
            let return_type = function.get_type().get_return_type();
            match return_type {
                Some(return_type_enum) => Value::new(
                    return_type_enum,
                    function.as_global_value().as_basic_value_enum(),
                ),
                _ => panic!("Void assigned to variable, where?"), // Some(return_type) => (
                                                                  //     return_type.as_basic_type_enum().into(),
                                                                  //     function.as_global_value().as_basic_value_enum(),
                                                                  // ),
                                                                  // None => (
                                                                  //     CartType::void(self.context),
                                                                  //     function.as_global_value().as_basic_value_enum(),
                                                                  // ),
            }
        } else {
            // Then standard variable in the symbol table.
            // It exists, as verified by the type checker.
            let mut var_alloca = *self.symbol_table.get(name).unwrap();
            self.as_r_value(&mut var_alloca);
            let loaded = self
                .builder
                .build_load(
                    var_alloca.type_enum,
                    var_alloca.basic_value.into_pointer_value(),
                    format!(
                        "loaded_{}",
                        var_alloca.basic_value.get_name().to_str().unwrap()
                    )
                    .as_str(),
                )
                .unwrap();
            Value::new(var_alloca.type_enum, loaded.as_basic_value_enum())
        }
    }
}
