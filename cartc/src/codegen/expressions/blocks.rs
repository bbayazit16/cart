use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use crate::codegen::CodeGen;
use crate::codegen::value::{RValue, Value};
use crate::hir::Block;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for a block.
    /// Optionally, pass a tuple of values that will be added as variables
    /// to the symbol table after starting the scope.
    /// 
    /// The input variables are always R-Values. Consider an input variable `x` of type `i32`.
    /// The variable cannot be an L-Value because it is not an alloca.
    /// 
    /// The output value is always an R-Value. If it was an L-Value, it would be possible to
    /// return a stack pointer that is no longer valid after the function returns.
    pub(crate) fn generate_block(
        &mut self,
        block: &Block,
        variables: Vec<(&String, Value<'ctx, RValue>)>,
    ) -> Option<Value<'ctx, RValue>> {
        {
            self.symbol_table.begin_scope();

            for (name, variable) in variables {
                // The parameters we are working with at this point in `variables` are function
                // parameters. For each function parameter, we have to:
                //      1) Create an alloca for the parameter, if it is not a pointer.
                //      2) Store the parameter in the alloca.
                //      3) Add the parameter to the symbol table.
                // This way, we can directly reference the parameter in the function body as
                // a pointer (an l-value). This makes the function parameters mutable and
                // improves the flexibility. By mem2reg optimization, the parameters will already
                // be promoted to registers if possible either way.
                
                // 1) Create an alloca for the parameter.
                let alloca = self.create_entry_block_alloca(
                    BasicTypeEnum::from(variable),
                    format!("alloca_input_var_{}", name).as_str(),
                );
                
                // 2) Store the parameter in the alloca.
                self.builder
                    .build_store(alloca, BasicValueEnum::from(variable))
                    .unwrap();
                
                // 3) Add the parameter to the symbol table.
                let value = Value::new_l(BasicTypeEnum::from(variable), alloca.as_basic_value_enum());
                self.symbol_table.add(name.to_string(), value);
            }

            for declaration in block.declarations.iter() {
                self.generate_declaration(declaration);
            }
        }

        let return_value = block.return_expr.as_ref().and_then(|expr_| {
            self.generate_expression(expr_).map(|v| {
                // Here, we have generated the return expression. If the return expression is an
                // l-value, we have to load the value from the l-value and return the r-value.
                self.cast_to_r_value(v)
            })
        });

        self.symbol_table.end_scope();
        
        return_value
    }
}
