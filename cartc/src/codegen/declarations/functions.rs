use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::Function;
use inkwell::values::{BasicValue, BasicValueEnum};

impl CodeGen<'_> {
    /// Generates the LLVM IR for a function declaration.
    ///
    /// Steps to generate a function are as follows:
    ///      1) Create a function type.
    ///      2) Add the function to the module.
    ///      3) Add the function parameters to the symbol table.
    ///      4) Create a basic block for the function.
    ///      5) Generate the block.
    ///      6) Return the block's generated value, if any.
    pub(super) fn generate_function(&mut self, function_hir: &Function) {
        // 1) Create a function type.
        let function_type = self.create_function_type(
            &function_hir.signature.return_type,
            &function_hir
                .signature
                .params
                .iter()
                .map(|(_, ty)| ty)
                .collect::<Vec<_>>(),
        );

        // 2) Add the function to the module.
        let function = self.module.add_function(
            &function_hir.signature.mangled_name,
            function_type,
            None, // Linkage::External
        );

        // 3) Add the function parameters to the symbol table.
        let mut variables_to_add = Vec::new();
        for (i, param) in function.get_param_iter().enumerate() {
            let hir_param = &function_hir.signature.params[i];
            let name = &hir_param.0;
            param.set_name(name);
            variables_to_add.push((
                name,
                Value::new_r(param.get_type(), param.as_basic_value_enum()),
            ));
        }

        // 4) Create a basic block for the function.
        let bb_name = format!("{}-entry", &function_hir.signature.mangled_name);
        let basic_block = self.context.append_basic_block(function, &bb_name);
        self.builder.position_at_end(basic_block);

        // 5) Generate the block.
        let basic_value_enum = self.generate_block(&function_hir.body, variables_to_add);

        // 6) Return the block's generated value, if any.
        if let Some(return_r_value) = basic_value_enum {
            let return_block = self.context.append_basic_block(function, "return_block");
            self.builder
                .build_unconditional_branch(return_block)
                .unwrap();
            self.builder.position_at_end(return_block);
            self.builder
                .build_return(Some(&BasicValueEnum::from(return_r_value)))
                .unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }
    }
}
