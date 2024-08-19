use crate::ast::{Block, CallExpr, Expr, IfExpr, Literal, StructAccessExpr, StructLiteral};
use crate::codegen::cart_type::CartType;
use crate::codegen::symbol_table::Variable;
use crate::codegen::CodeGen;
use crate::token::Token;
use crate::token_value;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, IntValue};
use inkwell::IntPredicate;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for an expression.
    pub(super) fn generate_expression(
        &mut self,
        expr: &Expr,
    ) -> Option<(CartType<'ctx>, BasicValueEnum<'ctx>)> {
        match expr {
            Expr::Block(ref block) => self.generate_block(block, Vec::new()),
            Expr::Literal(ref literal) => self.generate_literal(literal),
            Expr::Binary(ref binary_expr) => Some(self.generate_binary(binary_expr)),
            Expr::Variable(ref token, ..) => Some(self.generate_variable(token)),
            Expr::Call(ref call_expr) => self.generate_call_expr(call_expr),
            Expr::If(ref if_expr) => Some(self.generate_if_expr(if_expr)),
            Expr::StructLiteral(ref struct_literal) => {
                Some(self.generate_struct_literal(struct_literal))
            }
            Expr::StructAccess(ref struct_access) => {
                Some(self.generate_struct_access(struct_access))
            }
            _ => todo!(),
        }
    }

    /// Generates the LLVM IR for a block.
    /// Optionally, pass a tuple of values that will be added as variables
    /// to the symbol table after starting the scope.
    pub(super) fn generate_block(
        &mut self,
        block: &Block,
        variables: Vec<(String, Variable<'ctx>)>,
    ) -> Option<(CartType<'ctx>, BasicValueEnum<'ctx>)> {
        {
            self.symbol_table.start_scope();

            for (name, variable) in variables {
                self.symbol_table.add_variable(name, variable);
            }

            for declaration in block.declarations.iter() {
                self.generate_declaration(declaration);
            }
        }

        match &block.return_expr {
            Some(expr_) => {
                let (ty, value) = self.generate_expression(expr_)?;
                self.symbol_table.end_scope();

                if ty.is_alloca {
                    let loaded = self
                        .builder
                        .build_load(ty.type_enum, value.into_pointer_value(), "loaded_value")
                        .expect("Failed to load value");

                    Some((ty, loaded))
                } else {
                    Some((ty, value))
                }
            }
            None => {
                self.symbol_table.end_scope();
                None
            }
        }
    }

    /// Generates the LLVM IR for a literal.
    fn generate_literal(
        &self,
        literal: &Literal,
    ) -> Option<(CartType<'ctx>, BasicValueEnum<'ctx>)> {
        match literal {
            Literal::Number(token) => {
                let value = token_value!(token).parse::<i32>().unwrap();
                let value = self
                    .context
                    .i32_type()
                    .const_int(value as u64, false)
                    .into();
                Some((
                    BasicTypeEnum::IntType(self.context.i32_type()).into(),
                    value,
                ))
            }
            _ => unimplemented!("Other literal types"),
        }
    }

    /// Generates the LLVM IR for binary expressions.
    fn generate_binary(
        &mut self,
        binary_expr: &crate::ast::BinaryExpr,
    ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
        let (left_type, left_value) = self.generate_expression(&binary_expr.left).unwrap();
        let (right_type, right_value) = self.generate_expression(&binary_expr.right).unwrap();
        let operator = &binary_expr.operator;

        let (left_type, left_value) = if left_type.is_alloca {
            let ty_enum = left_type.type_enum;
            (
                left_type.without_alloca(),
                self.builder
                    .build_load(ty_enum, left_value.into_pointer_value(), "loaded_left")
                    .expect("Failed to load left value"),
            )
        } else {
            (left_type, left_value)
        };
        
        let (right_type, right_value) = if right_type.is_alloca {
            let ty_enum = right_type.type_enum;
            (
                right_type.without_alloca(),
                self.builder
                    .build_load(ty_enum, right_value.into_pointer_value(), "loaded_right")
                    .expect("Failed to load right value"),
            )
        } else {
            (right_type, right_value)
        };

        if left_type != right_type || left_type.name() != right_type.name() {
            panic!("Binary expression types do not match");
        }

        match left_type.into() {
            BasicTypeEnum::IntType(_) => {
                let left = left_value.into_int_value();
                let right = right_value.into_int_value();
                (
                    right_type,
                    self.generate_int_binary(left, right, operator)
                        .as_basic_value_enum(),
                )
            }
            _ => unimplemented!("Binary expressions for non-int types"),
        }
    }

    /// Generates LLVM IR for integer binary expressions.
    fn generate_int_binary(
        &mut self,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
        operator: &Token,
    ) -> IntValue<'ctx> {
        match operator {
            Token::Plus(..) => self.builder.build_int_add(left, right, "add").unwrap(),
            Token::Minus(..) => self.builder.build_int_sub(left, right, "sub").unwrap(),
            Token::Star(..) => self.builder.build_int_mul(left, right, "mul").unwrap(),
            Token::Slash(..) => self
                .builder
                .build_int_unsigned_div(left, right, "div")
                .unwrap(),
            Token::EqualEqual(..) => self
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "eq")
                .unwrap(),
            Token::LessEqual(..) => self
                .builder
                .build_int_compare(IntPredicate::SLE, left, right, "sle")
                .unwrap(),
            Token::LeftAngle(..) => self
                .builder
                .build_int_compare(IntPredicate::SLT, left, right, "slt")
                .unwrap(),
            Token::GreaterEqual(..) => self
                .builder
                .build_int_compare(IntPredicate::SGE, left, right, "sge")
                .unwrap(),
            Token::RightAngle(..) => self
                .builder
                .build_int_compare(IntPredicate::SGT, left, right, "sgt")
                .unwrap(),
            Token::Percent(..) => self
                .builder
                .build_int_signed_rem(left, right, "srem")
                .unwrap(),
            _ => unimplemented!("Other binary operators"),
        }
    }

    /// Generates LLVM IR for variable expressions.
    fn generate_variable(&self, token: &Token) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
        let name = token_value!(token);
        if let Some(function) = self.module.get_function(&name) {
            (
                function
                    .get_type()
                    .get_return_type()
                    .unwrap()
                    .as_basic_type_enum()
                    .into(),
                function.as_global_value().as_basic_value_enum(),
            )
        } else if let Some(variable) = self.symbol_table.get_variable(&name) {
            match variable {
                Variable::Immutable(pointee_type, pointer_value)
                | Variable::Mutable(pointee_type, pointer_value) => {
                    (
                        pointee_type.clone().with_alloca(),
                        pointer_value.as_basic_value_enum(),
                    )
                    // let basic_type_enum: BasicTypeEnum = pointee_type.clone().into();
                    // (
                    //     pointee_type.clone(),
                    //     self.builder
                    //         .build_load(basic_type_enum, *pointer_value, &name)
                    //         .expect("Failed to load variable"),
                    // )
                }
                Variable::StructDecl(_, _) => panic!("Struct declarations can't be values"),
                Variable::ImmutableParam(ref param_type, param) => (param_type.clone(), *param),
            }
        } else {
            panic!("Variable `{}` not found", name);
        }
    }

    /// Generates LLVM IR for call expressions.
    fn generate_call_expr(
        &mut self,
        call_expr: &CallExpr,
    ) -> Option<(CartType<'ctx>, BasicValueEnum<'ctx>)> {
        let (_, callee_expr) = self
            .generate_expression(&call_expr.callee)
            .expect("Callee expression not found");

        // callee_expr.get_name();
        // Returns variable name ^
        let callee = self
            .module
            .get_function(callee_expr.get_name().to_str().unwrap())
            .expect("Callee function not found");

        if callee.count_params() != call_expr.arguments.len() as u32 {
            panic!("Incorrect # of arguments")
        }

        let args: Vec<BasicMetadataValueEnum> = call_expr
            .arguments
            .iter()
            .map(|arg| {
                // self.generate_expression(arg).unwrap().1.into()
                let (arg_type, arg_value) = self.generate_expression(arg).unwrap();
                if arg_type.is_alloca {
                    // TODO: pass by reference for some types
                    // load the value
                    let loaded = self
                        .builder
                        .build_load(
                            arg_type.type_enum,
                            arg_value.into_pointer_value(),
                            "loaded_var",
                        )
                        .expect("Failed to load value");
                    loaded.into()
                } else {
                    arg_value.into()
                }
            })
            .collect();

        if callee.get_type().get_return_type().is_none() {
            None
        } else {
            Some((
                callee
                    .get_type()
                    .get_return_type()
                    .expect("Return type not found")
                    .as_basic_type_enum()
                    .into(),
                self.builder
                    .build_call(callee, &args, "call")
                    .expect("Failed to build call")
                    .try_as_basic_value()
                    .left()
                    .expect("Return type unsupported yet"),
            ))
        }
    }

    /// Generates LLVM IR for if expressions.
    fn generate_if_expr(&mut self, if_expr: &IfExpr) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
        // After lowering, it is ensured that there is always and else branch,
        // and that elif statements, if any, are within the else branch.
        let function = self
            .builder
            .get_insert_block()
            .expect("No insertion block")
            .get_parent()
            .expect("No parent");

        let then_block = self.context.append_basic_block(function, "then");
        let else_block = self.context.append_basic_block(function, "else");
        let exit_block = self.context.append_basic_block(function, "exit");

        let (_, cond_value) = self
            .generate_expression(&if_expr.condition)
            .expect("Failed to generate conditional value");

        self.builder
            .build_conditional_branch(cond_value.into_int_value(), then_block, else_block)
            .unwrap();

        self.builder.position_at_end(then_block);
        let (then_type, then_value) = self
            .generate_block(&if_expr.then_branch, Vec::new())
            .expect("Can't generate then block");
        self.builder.build_unconditional_branch(exit_block).unwrap();

        self.builder.position_at_end(else_block);

        let else_value = if let Some(ref else_block) = if_expr.else_branch {
            self.generate_block(else_block, Vec::new())
        } else {
            None
        };

        if let Some(else_expr) = else_value.clone() {
            if then_type != else_expr.0 || then_type.name() != else_expr.0.name() {
                panic!("If and else block types must match");
            }
        }

        self.builder.build_unconditional_branch(exit_block).unwrap();

        self.builder.position_at_end(exit_block);
        let phi_node = self
            .builder
            .build_phi(self.context.i32_type(), "if_phi")
            .unwrap();

        phi_node.add_incoming(&[(&then_value, then_block)]);
        if let Some(else_value) = else_value {
            phi_node.add_incoming(&[(&else_value.1, else_block)]);
        }

        // then_type == else_value.0
        (then_type, phi_node.as_basic_value())
    }

    /// Generates LLVM IR for struct literals.
    fn generate_struct_literal(
        &mut self,
        struct_literal: &StructLiteral,
    ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
        let struct_literal_name = token_value!(&struct_literal.name);

        let (struct_type, field_to_index) = {
            // TODO: Try to remove clone...
            let variable = self
                .symbol_table
                .get_variable(&struct_literal_name)
                .expect("Unknown struct")
                .clone();
            match variable {
                Variable::StructDecl(struct_type, field_to_index) => (struct_type, field_to_index),
                _ => panic!("Not a struct"),
            }
        };

        let twine = format!("struct_literal_{}", struct_literal_name);
        let struct_ptr = self.create_entry_block_alloca(struct_type, &twine);

        for (field_name, field_expr) in struct_literal.fields.iter() {
            let field_name = token_value!(field_name);
            let (field_index, _) = field_to_index.get(&field_name).expect("Unknown field");
            let (_, field_expr_value) = self
                .generate_expression(field_expr)
                .expect("Failed to generate field expression");

            let gep = self
                .builder
                .build_struct_gep(struct_type, struct_ptr, *field_index as u32, &field_name)
                .expect("Failed to build GEP");
            self.builder
                .build_store(gep, field_expr_value)
                .expect("Failed to store field");
        }

        // let struct_value = self
        //     .builder
        //     .build_load(
        //         struct_type,
        //         struct_ptr,
        //         &format!("{}.value", struct_literal_name),
        //     )
        //     .expect("Failed to load struct");
        //
        // let cart_type: CartType = struct_type.as_basic_type_enum().into();
        // (cart_type.with_name(struct_literal_name), struct_value)

        // Return the pointer to the struct
        let cart_type: CartType = struct_type.as_basic_type_enum().into();
        (
            cart_type.with_name(struct_literal_name).with_alloca(),
            struct_ptr.as_basic_value_enum(),
        )
    }

    /// Generates LLVM IR for struct access expressions.
    fn generate_struct_access(
        &mut self,
        struct_access: &StructAccessExpr,
    ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
        // Fields: struct_access_expr.fields. For example if there's a struct `User` assigned to
        // `user`, then `user.name.first` would be represented as [name, first].
        // Object: struct_access_expr.object. This is the struct object itself.
        // TODO: Support multiple fields
        let field_name = token_value!(&struct_access.fields[0]);
        let (cart_struct_type, struct_value_or_ptr) = self
            .generate_expression(&struct_access.object)
            .expect("Can't generate object");

        let struct_object_name = cart_struct_type.name().expect("Can't retrieve name");

        let struct_ptr = match cart_struct_type.is_alloca {
            true => struct_value_or_ptr.into_pointer_value(),
            false => {
                let basic_type_enum: BasicTypeEnum = cart_struct_type.clone().into();
                let struct_ptr =
                    self.create_entry_block_alloca(basic_type_enum, struct_object_name);
                self.builder
                    .build_store(struct_ptr, struct_value_or_ptr)
                    .unwrap();
                struct_ptr
            }
        };

        let (index, field_type) = match self
            .symbol_table
            .get_variable(struct_object_name)
            .expect("Can't retrieve variable")
        {
            Variable::StructDecl(_, field_to_index) => {
                field_to_index.get(&field_name).expect("Unknown field")
            }
            _ => panic!("Not a struct"),
        };

        let bte: BasicTypeEnum = cart_struct_type.into();
        let gep = self
            .builder
            .build_struct_gep(bte, struct_ptr, *index as u32, &field_name)
            .expect("Failed to build GEP");

        (field_type.clone().with_alloca(), gep.as_basic_value_enum())

        // let gep = self.builder.build_struct_gep(
        //     cart_struct_type.into(),
        //     struct_ptr,
        //     index as u32,
        //     &field_name,
        // ).expect("Failed to build GEP");
    }
    // fn generate_struct_access(
    //     &mut self,
    //     struct_access_expr: &StructAccessExpr,
    // ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     println!("Starting struct access\n{}", &self.module.print_to_string().to_string());
    //     // Fields: struct_access_expr.fields. For example if there's a struct `User` assigned to
    //     // `user`, then `user.name.first` would be represented as [name, first].
    //     // Object: struct_access_expr.object. This is the struct object itself.
    //     // TODO: Change to generate_expression in the future
    //     let (cart_struct_type, struct_value_or_ptr) = self
    //         .generate_expression(&struct_access_expr.object)
    //         .expect("Can't generate object");
    //
    //     let struct_object_name = cart_struct_type.name().expect("Can't retrieve name");
    //     let basic_type_enum: BasicTypeEnum = cart_struct_type.clone().into();
    //     let struct_type = basic_type_enum.into_struct_type();
    //
    //     let (struct_object_value, struct_ptr) = match struct_value_or_ptr {
    //         BasicValueEnum::PointerValue(ref ptr) => {
    //             let struct_value = self
    //                     .builder
    //                     .build_load(struct_type, *ptr, "struct_object")
    //                     .expect("Failed to load struct object")
    //                     .into_struct_value();
    //             (struct_value, *ptr)
    //         },
    //         BasicValueEnum::StructValue(value) => {
    //             let struct_ptr = self.create_entry_block_alloca(basic_type_enum, struct_object_name);
    //             (value, struct_ptr)
    //         },
    //         _ => panic!("Invalid struct object"),
    //     };
    //
    //     let struct_decl = self
    //         .symbol_table
    //         .get_variable(struct_object_name)
    //         .expect("Can't retrieve variable");
    //     let field_to_index = match struct_decl {
    //         Variable::StructDecl(_, field_to_index) => field_to_index,
    //         _ => panic!("Not a struct"),
    //     };
    //     // Iterate like a LinkedList
    //     let mut current_struct_value = struct_object_value;
    //     let mut current_struct_ptr = struct_ptr;
    //     // let mut current_struct_ptr = self
    //     //     .builder
    //     //     .build_alloca(
    //     //         current_struct_value.get_type(),
    //     //         &format!("{}.ptr", struct_object_name),
    //     //     )
    //     //     .expect("Failed to allocate struct pointer");
    //     let field_count = struct_access_expr.fields.len();
    //     for (i, field) in struct_access_expr.fields.iter().enumerate() {
    //         let field_name = token_value!(field);
    //         let field_index = *field_to_index.get(&field_name).expect("Unknown field");
    //
    //         let field_pointer = self
    //             .builder
    //             .build_struct_gep(
    //                 current_struct_value.get_type(),
    //                 current_struct_ptr,
    //                 field_index as u32,
    //                 &format!("{}.{}", struct_object_name, field_name),
    //             )
    //             .expect("Failed to build GEP");
    //
    //         // If not the last field:
    //         if i != field_count - 1 {
    //             let field_value = self
    //                 .builder
    //                 .build_load(
    //                     field_pointer.get_type(),
    //                     field_pointer,
    //                     &format!("{}.{}", struct_object_name, field_name),
    //                 )
    //                 .expect("Failed to load field");
    //
    //             current_struct_value = field_value.into_struct_value();
    //         }
    //         current_struct_ptr = field_pointer;
    //     }
    //
    //     let field_value = self
    //         .builder
    //         .build_load(
    //             current_struct_value.get_type(),
    //             current_struct_ptr,
    //             &format!(
    //                 "{}.{}",
    //                 struct_object_name,
    //                 token_value!(&struct_access_expr.fields.last().unwrap())
    //             ),
    //         )
    //         .expect("Failed to load field");
    //
    //     (field_value.get_type().into(), field_value)
    // }
}
