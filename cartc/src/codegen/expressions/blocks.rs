use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::Block;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for a block, regardless of r-value or l-value.
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
        variables: Vec<(&String, Value<'ctx>, Option<BasicTypeEnum<'ctx>>, bool)>,
    ) -> Option<Value<'ctx>> {
        {
            self.symbol_table.begin_scope();

            for (name, variable, original_type_or_none, is_self) in variables {
                if is_self {
                    // If the variable is `self`, just skip everything, add to the symbol table.
                    let value = Value::new(
                        BasicTypeEnum::from(variable),
                        BasicValueEnum::from(variable)
                    );
                    self.symbol_table.add(name.to_string(), value);
                } else {
                    // The parameters we are working with at this point in `variables` are function
                    // parameters. For each function parameter, we have to:
                    //      1) Create an alloca for the parameter. This is equal to
                    //         the original type (if reference type).
                    //      2) Store the parameter in the alloca. If the parameter is a
                    //         reference type, then we must also load it before storing.
                    //      3) Add the parameter to the symbol table. This is equal to
                    //         the original type (if reference type).
                    // This way, we can directly reference the parameter in the function body as
                    // a pointer (an l-value). This makes the function parameters mutable and
                    // improves the flexibility. By mem2reg optimization, the parameters will already
                    // be promoted to registers if possible either way.

                    let original_type = original_type_or_none.unwrap_or(BasicTypeEnum::from(variable));
                    // 1) Create an alloca for the parameter.
                    let alloca = self.create_entry_block_alloca(
                        original_type,
                        format!("alloca_input_var_{}", name).as_str(),
                    );

                    // 2) Store the parameter in the alloca.
                    let value_to_store = if let Some(original_type) = original_type_or_none {
                        self.builder
                            .build_load(
                                original_type,
                                BasicValueEnum::from(variable).into_pointer_value(),
                                &format!("reftype_{}_load", name),
                            )
                            .unwrap()
                    } else {
                        BasicValueEnum::from(variable)
                    };

                    self.builder.build_store(alloca, value_to_store).unwrap();

                    // 3) Add the parameter to the symbol table.
                    let value = Value::new(original_type, alloca.as_basic_value_enum());
                    self.symbol_table.add(name.to_string(), value);
                }
            }

            for declaration in block.declarations.iter() {
                self.generate_declaration(declaration);
            }
        }

        let return_value = block.return_expr.as_ref().and_then(|expr_| {
            // Blocks must always return an r-value. Otherwise, this would lead to weird conditions
            // such as being able to assign from a function such as when: returns_l_value() = 4;
            self.generate_expression_r_value(expr_)
        });

        self.symbol_table.end_scope();

        return_value
    }
}
