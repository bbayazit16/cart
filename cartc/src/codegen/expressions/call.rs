use crate::codegen::value::{Value};
use crate::codegen::CodeGen;
use crate::hir::{Expression, Type};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for call expressions.
    ///
    /// The result of a call expression is always an R-Value, as returning from the stack
    /// pointer is not valid.
    ///
    /// Steps to generate a call expression:
    ///     1) Get the function from the module.
    ///     2) Generate the arguments. Each one of the arguments must be an R-Value.
    ///     3) Execute the call instruction.
    ///     4) If the return type is not unit, return the value, as an R-Value.
    pub(super) fn generate_call_expr_r_value(
        &mut self,
        callee: &String,
        arguments: &[Expression],
        return_type: &Type,
    ) -> Option<Value<'ctx>> {
        // TODO: Forbid redefinition of std publicly facing function in type checker
        // 1) Get the function from the module.
        let callee_fn = self
            .module
            .get_function(callee)
            .unwrap_or_else(|| panic!("Function {} not found", callee));

        let param_types = callee_fn.get_type().get_param_types();

        // 2) Generate the arguments.
        let args: Vec<BasicMetadataValueEnum> = arguments
            .iter()
            .enumerate()
            .map(|(param_index, arg)| {
                let r_value = self.generate_expression_r_value(arg).unwrap();
                // TODO: Check the code below after the rl-value refactor with counterexamples + reformat code
                // If the argument requires a pointer, create an entry block alloca, and
                // store the value. Then, pass the alloca to the function.
                // This is needed when the arguments are reference types, such as strings.
                let param_type = param_types[param_index];
                if param_type.is_pointer_type() {
                    // If pass by reference (like a string)
                    // Then create a new pointer to the data, and pass that into the function.
                    let alloca = self.create_entry_block_alloca(
                        BasicTypeEnum::from(r_value),
                        format!("alloca_{}_{}", callee, param_index).as_str(),
                    );
                    self.builder
                        .build_store(alloca, BasicValueEnum::from(r_value))
                        .unwrap();
                    
                    alloca.into()
                    // TODO: Cleanup comments
                    // match value {
                    //     ValueState::L(l_value) => {
                    //         let original_type = BasicTypeEnum::from(l_value);
                    //         if original_type.is_pointer_type() {
                    //             BasicValueEnum::from(self.cast_to_r_value(value)).into()
                    //         } else {
                    //             PointerValue::from(l_value).into()
                    //         }
                    //         // let r_value = self.cast_to_r_value(value);
                    //         // BasicValueEnum::from(r_value).into()
                    //     }
                    //     ValueState::R(r_value) => {
                    //         let alloca = self.create_entry_block_alloca(
                    //             BasicTypeEnum::from(r_value),
                    //             format!("alloca_{}_{}", callee, param_index).as_str(),
                    //         );
                    //         self.builder
                    //             .build_store(alloca, BasicValueEnum::from(r_value))
                    //             .unwrap();
                    //         alloca.into()
                    //     }
                    // }
                } else {
                    // let r_value = self.cast_to_r_value(value);
                    // Pass by value
                    BasicValueEnum::from(r_value).into()
                }
            })
            .collect();

        // 3) Execute the call instruction.
        let call_site = self
            .builder
            .build_call(callee_fn, &args, format!("call_{}", callee).as_str())
            .unwrap();

        // 4) If the return type is not unit, return the value.
        match return_type {
            Type::Unit => None,
            _ => Some(Value::new(
                self.to_basic_type_enum(return_type).unwrap(),
                call_site.try_as_basic_value().unwrap_left(),
            )),
        }
    }
}
