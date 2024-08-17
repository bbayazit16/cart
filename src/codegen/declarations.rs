use crate::ast::{Declaration, FunctionDecl};
use crate::codegen::symbol_table::Variable;
use crate::codegen::types::create_function_type;
use crate::codegen::CodeGen;
use inkwell::values::BasicValue;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for a declaration.
    pub(super) fn generate_declaration(&mut self, declaration: &Declaration) {
        match declaration {
            Declaration::FunctionDecl(ref func_decl) => self.generate_function(func_decl),
            Declaration::StatementDecl(ref stmt) => self.generate_statement(stmt),
            _ => todo!(),
        };
    }

    /// Generates the LLVM IR for a function declaration.
    fn generate_function(&mut self, function_decl: &FunctionDecl) {
        let function_type = create_function_type(
            self.context,
            function_decl.return_type.to_llvm_type(self.context),
            function_decl
                .params
                .iter()
                .map(|p| p.param_type.to_llvm_type(self.context))
                .collect(),
        );

        let function = self.module.add_function(
            &crate::token_value!(&function_decl.name),
            function_type,
            None, // Linkage::External
        );

        let mut variables_to_add = Vec::new();
        for (i, param) in function.get_param_iter().enumerate() {
            let name = crate::token_value!(&function_decl.params[i].name);
            param.set_name(&name);
            variables_to_add.push((name, Variable::Immutable(param)));
        }

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let basic_value_enum = self.generate_block(&function_decl.body, variables_to_add);
        // dbg!(&basic_value_enum);
        let basic_value = basic_value_enum.as_ref().map(|val| val as &dyn BasicValue);
        // dbg!(&basic_block.get_terminator());
        self.builder.build_return(basic_value).unwrap();

        // // If there's a return value that's already generated, return that value.
        // if basic_block.get_terminator().is_none() {
        //     // No need to handle no return values here: since basic_value is still an Option
        //     // at this point, and build_return expects an Option<BasicValue>, we can just pass
        //     // basic_value directly.
        //     // Even if there's no return statement inside the block, and there's no last
        //     // value, this branch will run self.builder.build_return(None).
        //     self.builder.build_return(basic_value).unwrap();
        // }
    }
}
