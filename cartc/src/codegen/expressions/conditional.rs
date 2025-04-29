use crate::codegen::value::{Value};
use crate::codegen::CodeGen;
use crate::hir::{Block, Expression, Type};
use inkwell::values::BasicValueEnum;

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for if expressions.
    ///
    /// The generated value must be an R-Value. Cases such as:
    /// ```cart
    /// if condition { a } else { b } = 4;
    /// ```
    /// are deemed invalid and will not be supported. This must be handled in the parser or
    /// the type checker before the code generation phase. This function only assumes that
    /// R-Values can be generated from the if expressions.
    ///
    /// Steps to generate the if expression:
    ///     1) Create a result alloca if the type is not Unit.
    ///     2) Generate the condition value and append branches.
    ///     3) Build a conditional branch based on the condition value.
    ///     4) Generate the then block.
    ///     5) Store the result of the then block in the result alloca if not Unit.
    ///     6) Generate the else block if it exists.
    ///     7) Store the result of the else block in the result alloca if not Unit.
    ///     8) Build an unconditional branch to the continue block.
    ///     9) Position at the continue block.
    ///     10) Load the result alloca if not Unit.
    ///     11) Return the loaded value if not Unit.
    pub(super) fn generate_if_expr_r_value(
        &mut self,
        condition: &Expression,
        then_branch: &Block,
        else_branch: &Option<Box<Block>>,
        ty: &Type,
    ) -> Option<Value<'ctx>> {
        // Useful note on phi nodes for the future:
        // https://stackoverflow.com/questions/67079122/how-to-produce-phi-instruction-in-clang-for-llvm-ir
        
        // If the type is Unit, then there is no result to return.
        let is_unit = matches!(ty, Type::Unit);
        
        // 1) Create a result alloca if the type is not Unit.
        let result_alloca = if !is_unit {
            Some(self.create_entry_block_alloca(
                self.to_basic_type_enum(ty).unwrap(),
                "conditional_result",
            ))
        } else {
            None
        };
        
        let function = self
            .builder
            .get_insert_block()
            .expect("No insertion block")
            .get_parent()
            .expect("No parent");
        
        // 2) Generate the condition value and append branches.
        let continue_block = self.context.append_basic_block(function, "continue");
        let condition_expr = self.generate_expression_r_value(condition).unwrap();
        // let condition_value = self.cast_to_r_value(condition_expr);
        let then_block = self.context.append_basic_block(function, "then");
        let else_block = else_branch
            .as_ref()
            .map(|_| self.context.append_basic_block(function, "else"));
        
        let initial_false_target = else_block.unwrap_or(continue_block);
        
        // 3) Build a conditional branch based on the condition value.
        self.builder
            .build_conditional_branch(
                BasicValueEnum::from(condition_expr).into_int_value(),
                then_block,
                initial_false_target,
            )
            .unwrap();
        
        self.builder.position_at_end(then_block);
        // 4) Generate the then block.
        let then_value = self.generate_block(then_branch, Vec::new());
        
        // 5) Store the result of the then block in the result alloca if not Unit.
        if let (Some(result_alloca), Some(then_val)) = (result_alloca, then_value) {
            self.builder
                .build_store(result_alloca, BasicValueEnum::from(then_val))
                .unwrap();
        }
        
        self.builder
            .build_unconditional_branch(continue_block)
            .unwrap();
        
        // 6) Generate the else block if it exists.
        if let Some(else_block_ast) = else_branch {
            let else_block = else_block.unwrap();
            self.builder.position_at_end(else_block);
        
            let else_value = self.generate_block(else_block_ast, Vec::new());
        
            // 7) Store the result of the else block in the result alloca if not Unit.
            if let (Some(result_alloca), Some(else_val)) = (result_alloca, else_value) {
                self.builder
                    .build_store(result_alloca, BasicValueEnum::from(else_val))
                    .unwrap();
            }
        
            // 8) Build an unconditional branch to the continue block.
            self.builder
                .build_unconditional_branch(continue_block)
                .unwrap();
        }
        
        // 9) Position at the continue block.
        self.builder.position_at_end(continue_block);
        
        // 10) Load the result alloca if not Unit.
        if !is_unit {
            result_alloca.map(|alloca| {
                let ty_bte = self.to_basic_type_enum(ty).unwrap();
                let loaded_value = self
                    .builder
                    .build_load(ty_bte, alloca, "conditional_result_load")
                    .unwrap();
        
                // 11) Return the loaded value if not Unit.
                Value::new(ty_bte, loaded_value)
            })
        } else {
            None
        }
    }
}
