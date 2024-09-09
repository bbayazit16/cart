use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{Block, Expression, Type};
use inkwell::values::BasicValue;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for an expression.
    pub(super) fn generate_expression(&mut self, expr: &Expression) -> Option<Value<'ctx>> {
        match expr {
            Expression::Block(ref block) => self.generate_block(block, Vec::new()),
            Expression::Literal { value, ty } => Some(self.generate_literal(value, ty)),
            // Expr::Binary(ref binary_expr) => Some(self.generate_binary(binary_expr)),
            Expression::Variable { name, ty } => Some(self.generate_variable(name, ty)),
            // Expr::Call(ref call_expr) => self.generate_call_expr(call_expr),
            // Expr::If(ref if_expr) => Some(self.generate_if_expr(if_expr)),
            // Expr::StructLiteral(ref struct_literal) => {
            //     Some(self.generate_struct_literal(struct_literal))
            // }
            // Expr::StructAccess(ref struct_access) => {
            //     Some(self.generate_struct_access(struct_access))
            // }
            // Expr::Assignment(ref assignment) => Some(self.generate_assignment(assignment)),
            // Expr::MethodCall(ref method_call) => self.generate_method_call(method_call),
            // Expr::ArrayLiteral(ref expressions) => Some(self.generate_array_literal(expressions)),
            // Expr::ArrayAccess(ref array_access) => Some(self.generate_array_access(array_access)),
            e => {
                dbg!(&e);
                unimplemented!()
            }
        }
    }

    /// Generates the LLVM IR for a block.
    /// Optionally, pass a tuple of values that will be added as variables
    /// to the symbol table after starting the scope.
    pub(super) fn generate_block(
        &mut self,
        block: &Block,
        variables: Vec<(&String, Value<'ctx>)>,
    ) -> Option<Value<'ctx>> {
        // let alloca = if block.return_expr.is_some() {
        //     Some(
        //         self.create_entry_block_alloca(
        //             // Unwrapping is safe: return_expr.is_some() implies return_type is not void,
        //             // which is otherwise when to_basic_type_enum returns None.
        //             block
        //                 .return_type
        //                 .to_basic_type_enum(self.context)
        //                 .expect("return type err"),
        //             "return_value_alloca",
        //         ),
        //     )
        // } else {
        //     None
        // };

        {
            self.symbol_table.begin_scope();

            for (name, variable) in variables {
                self.symbol_table.add(name.to_string(), variable);
            }

            for declaration in block.declarations.iter() {
                self.generate_declaration(declaration);
            }
        }

        let value = match &block.return_expr {
            Some(expr_) => {
                match self.generate_expression(expr_) {
                    Some(expr_value) => {
                        todo!()
                    },
                    None => None,
                }
                // let expr_value = self.generate_expression(expr_);
                // // If there is a return expression that isn't unit type, create an entry block
                // // alloca, and return the return value.
                // let alloca = self.create_entry_block_alloca(
                //     // Unwrapping is safe: return_expr.is_some() implies return_type is not void,
                //     // which is otherwise when to_basic_type_enum returns None.
                //     block
                //         .return_type
                //         .to_basic_type_enum(self.context)
                //         .expect("return type err"),
                //     "return_value_alloca",
                // );
                // 
                // self.builder
                //     .build_store(alloca, expr_value.unwrap().basic_value)
                //     .unwrap();
            }
            None => None,
        };

        self.symbol_table.end_scope();
        value
    }

    /// Generates the LLVM IR for a literal.
    fn generate_literal(&self, value: &str, ty: &Type) -> Value<'ctx> {
        let literal_value = match ty {
            Type::Int => self
                .context
                .i32_type()
                .const_int(Self::int_value(value), false)
                .as_basic_value_enum(),
            Type::Int64 => self
                .context
                .i64_type()
                .const_int(Self::int_value(value), false)
                .as_basic_value_enum(),
            Type::Int128 => todo!(),
            Type::Int256 => todo!(),
            Type::Float => self
                .context
                .f32_type()
                .const_float(value.parse::<f32>().unwrap() as f64)
                .as_basic_value_enum(),
            Type::Float64 => self
                .context
                .f64_type()
                .const_float(value.parse::<f64>().unwrap())
                .as_basic_value_enum(),
            Type::Bool => self
                .context
                .bool_type()
                .const_int(Self::bool_value(value), false)
                .as_basic_value_enum(),
            Type::String => todo!(),
            Type::Unit => unimplemented!("unit type"),
            Type::Array(_) => todo!(),
            Type::Struct(_) => todo!(),
            Type::Enum(_) => todo!(),
            Type::Error(_) => todo!(),
            Type::Generic(_, _) => todo!(),
            Type::DeclaredGeneric(_) => todo!(),
        };

        Value::new(
            // Why unwrapping is safe: should be verified by the type checker.
            ty.to_basic_type_enum(self.context).unwrap(),
            literal_value,
        )
        // match literal {
        //     Literal::Integer(ref token) => {
        //         let value = token_value!(token).parse::<i32>().unwrap();
        //         let value = self
        //             .context
        //             .i32_type()
        //             .const_int(value as u64, false)
        //             .into();
        //         Some((
        //             BasicTypeEnum::IntType(self.context.i32_type()).into(),
        //             value,
        //         ))
        //     }
        //     Literal::String(ref token) => {
        //         let value = token_value!(token).to_string();
        //         let string_ptr: PointerValue<'ctx> =
        //             CartString::from_string(self.context, &self.builder, &value);
        //         let bte =
        //             BasicTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default()));
        //         // CartType is not marked as alloca, because Strings are passed around by reference
        //         // by default.
        //         Some((bte.into(), string_ptr.as_basic_value_enum()))
        //     }
        //     _ => unimplemented!("Other literal types"),
        // }
    }

    /// Detects base and converts a string to an integer value.
    fn int_value(value: &str) -> u64 {
        if let Some(stripped) = value.strip_prefix("0x") {
            u64::from_str_radix(stripped, 16).unwrap()
        } else if let Some(stripped) = value.strip_prefix("0o") {
            u64::from_str_radix(stripped, 8).unwrap()
        } else if let Some(stripped) = value.strip_prefix("0b") {
            u64::from_str_radix(stripped, 2).unwrap()
        } else {
            value.parse().unwrap()
        }
    }

    /// Converts a string to a boolean value.
    fn bool_value(value: &str) -> u64 {
        match value {
            "true" => 1,
            "false" => 0,
            // This should be verified by the type checker.
            _ => unreachable!(
                "Invalid boolean value. An error must have occurred in the type checker"
            ),
        }
    }

    // /// Generates the LLVM IR for binary expressions.
    // fn generate_binary(
    //     &mut self,
    //     binary_expr: &crate::ast::BinaryExpr,
    // ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     let (left_type, left_value) = self.generate_expression(&binary_expr.left).unwrap();
    //     let (right_type, right_value) = self.generate_expression(&binary_expr.right).unwrap();
    //     let operator = &binary_expr.operator;
    //
    //     let (left_type, left_value) = if left_type.is_alloca {
    //         let ty_enum = left_type.type_enum;
    //         (
    //             left_type.without_alloca(),
    //             self.builder
    //                 .build_load(ty_enum, left_value.into_pointer_value(), "loaded_left")
    //                 .expect("Failed to load left value"),
    //         )
    //     } else {
    //         (left_type, left_value)
    //     };
    //
    //     let (right_type, right_value) = if right_type.is_alloca {
    //         let ty_enum = right_type.type_enum;
    //         (
    //             right_type.without_alloca(),
    //             self.builder
    //                 .build_load(ty_enum, right_value.into_pointer_value(), "loaded_right")
    //                 .expect("Failed to load right value"),
    //         )
    //     } else {
    //         (right_type, right_value)
    //     };
    //
    //     if left_type != right_type || left_type.name() != right_type.name() {
    //         panic!("Binary expression types do not match");
    //     }
    //
    //     match left_type.into() {
    //         BasicTypeEnum::IntType(_) => {
    //             let left = left_value.into_int_value();
    //             let right = right_value.into_int_value();
    //             (
    //                 right_type,
    //                 self.generate_int_binary(left, right, operator)
    //                     .as_basic_value_enum(),
    //             )
    //         }
    //         _ => unimplemented!("Binary expressions for non-int types"),
    //     }
    // }
    //
    // /// Generates LLVM IR for integer binary expressions.
    // fn generate_int_binary(
    //     &mut self,
    //     left: IntValue<'ctx>,
    //     right: IntValue<'ctx>,
    //     operator: &Token,
    // ) -> IntValue<'ctx> {
    //     match operator {
    //         Token::Plus(..) => self.builder.build_int_add(left, right, "add").unwrap(),
    //         Token::Minus(..) => self.builder.build_int_sub(left, right, "sub").unwrap(),
    //         Token::Star(..) => self.builder.build_int_mul(left, right, "mul").unwrap(),
    //         Token::Slash(..) => self
    //             .builder
    //             .build_int_unsigned_div(left, right, "div")
    //             .unwrap(),
    //         Token::EqualEqual(..) => self
    //             .builder
    //             .build_int_compare(IntPredicate::EQ, left, right, "eq")
    //             .unwrap(),
    //         Token::LessEqual(..) => self
    //             .builder
    //             .build_int_compare(IntPredicate::SLE, left, right, "sle")
    //             .unwrap(),
    //         Token::LeftAngle(..) => self
    //             .builder
    //             .build_int_compare(IntPredicate::SLT, left, right, "slt")
    //             .unwrap(),
    //         Token::GreaterEqual(..) => self
    //             .builder
    //             .build_int_compare(IntPredicate::SGE, left, right, "sge")
    //             .unwrap(),
    //         Token::RightAngle(..) => self
    //             .builder
    //             .build_int_compare(IntPredicate::SGT, left, right, "sgt")
    //             .unwrap(),
    //         Token::Percent(..) => self
    //             .builder
    //             .build_int_signed_rem(left, right, "srem")
    //             .unwrap(),
    //         _ => unimplemented!("Other binary operators"),
    //     }
    // }
    //
    /// Generates LLVM IR for variable expressions.
    fn generate_variable(&self, name: &String, ty: &Type) -> Value<'ctx> {
        if let Some(function) = self.module.get_function(&name) {
            let return_type = function.get_type().get_return_type();
            match return_type {
                Some(return_type_enum) => Value::new(
                    return_type_enum,
                    function.as_global_value().as_basic_value_enum(),
                ),
                _ => todo!(), // Some(return_type) => (
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
            *self.symbol_table.get(name).unwrap()
        }
        // let name = token_value!(token);
        // if let Some(function) = self.module.get_function(&name) {
        //     let return_type = function.get_type().get_return_type();
        //     match return_type {
        //         Some(return_type) => (
        //             return_type.as_basic_type_enum().into(),
        //             function.as_global_value().as_basic_value_enum(),
        //         ),
        //         None => (
        //             CartType::void(self.context),
        //             function.as_global_value().as_basic_value_enum(),
        //         ),
        //     }
        // } else if let Some(variable) = self.symbol_table.get(&name) {
        //     match variable {
        //         Variable::Immutable(pointee_type, pointer_value)
        //         | Variable::Mutable(pointee_type, pointer_value) => (
        //             pointee_type.clone().with_alloca(),
        //             pointer_value.as_basic_value_enum(),
        //         ),
        //         Variable::StructDecl(_, _) => panic!("Struct declarations can't be values"),
        //         Variable::ImmutableParam(ref param_type, param) => (param_type.clone(), *param),
        //     }
        // } else {
        //     panic!("Variable `{}` not found", name);
        // }
    }

    // /// Generates LLVM IR for call expressions.
    // fn generate_call_expr(
    //     &mut self,
    //     call_expr: &CallExpr,
    // ) -> Option<(CartType<'ctx>, BasicValueEnum<'ctx>)> {
    //     // Temporarily disabled!
    //     //
    //     // let (_, callee_expr) = self
    //     //     .generate_expression(&call_expr.callee)
    //     //     .expect("Callee expression not found");
    //     //
    //     // callee_expr.get_name();
    //     // Returns variable name ^
    //     // let callee = self
    //     //     .module
    //     //     .get_function(callee_expr.get_name().to_str().unwrap())
    //     //     .expect("Callee function not found");
    //     let function_name = token_value!(&call_expr.callee);
    //     let callee  = self
    //         .module
    //         .get_function(&function_name)
    //         .expect("Callee function not found");
    //
    //     if callee.count_params() != call_expr.arguments.len() as u32 {
    //         panic!("Incorrect # of arguments")
    //     }
    //
    //     let args: Vec<BasicMetadataValueEnum> = call_expr
    //         .arguments
    //         .iter()
    //         .map(|arg| {
    //             // self.generate_expression(arg).unwrap().1.into()
    //             let (arg_type, arg_value) = self.generate_expression(arg).unwrap();
    //             if arg_type.is_alloca {
    //                 // TODO: pass by reference for some types
    //                 // load the value
    //                 let loaded = self
    //                     .builder
    //                     .build_load(
    //                         arg_type.type_enum,
    //                         arg_value.into_pointer_value(),
    //                         "loaded_var",
    //                     )
    //                     .expect("Failed to load value");
    //                 loaded.into()
    //             } else {
    //                 arg_value.into()
    //             }
    //         })
    //         .collect();
    //
    //     let call_site = self
    //         .builder
    //         .build_call(callee, &args, "call")
    //         .expect("Failed to build call");
    //
    //     if let Some(return_type) = callee.get_type().get_return_type() {
    //         let return_type = return_type.as_basic_type_enum().into();
    //         let return_value = call_site
    //             .try_as_basic_value()
    //             .left()
    //             .expect("Return type unsupported yet");
    //         Some((return_type, return_value))
    //     } else {
    //         None
    //     }
    // }
    //
    // /// Generates LLVM IR for if expressions.
    // fn generate_if_expr(&mut self, if_expr: &IfExpr) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     // After lowering, it is ensured that there is always and else branch,
    //     // and that elif statements, if any, are within the else branch.
    //     let function = self
    //         .builder
    //         .get_insert_block()
    //         .expect("No insertion block")
    //         .get_parent()
    //         .expect("No parent");
    //
    //     let then_block = self.context.append_basic_block(function, "then");
    //     let else_block = self.context.append_basic_block(function, "else");
    //     let exit_block = self.context.append_basic_block(function, "exit");
    //
    //     let (_, cond_value) = self
    //         .generate_expression(&if_expr.condition)
    //         .expect("Failed to generate conditional value");
    //
    //     self.builder
    //         .build_conditional_branch(cond_value.into_int_value(), then_block, else_block)
    //         .unwrap();
    //
    //     self.builder.position_at_end(then_block);
    //     let (then_type, then_value) = self
    //         .generate_block(&if_expr.then_branch, Vec::new())
    //         .expect("Can't generate then block");
    //     self.builder.build_unconditional_branch(exit_block).unwrap();
    //
    //     self.builder.position_at_end(else_block);
    //
    //     let else_value = if let Some(ref else_block) = if_expr.else_branch {
    //         self.generate_block(else_block, Vec::new())
    //     } else {
    //         None
    //     };
    //
    //     if let Some(else_expr) = else_value.clone() {
    //         if then_type != else_expr.0 || then_type.name() != else_expr.0.name() {
    //             panic!("If and else block types must match");
    //         }
    //     }
    //
    //     self.builder.build_unconditional_branch(exit_block).unwrap();
    //
    //     self.builder.position_at_end(exit_block);
    //     let phi_node = self
    //         .builder
    //         .build_phi(self.context.i32_type(), "if_phi")
    //         .unwrap();
    //
    //     phi_node.add_incoming(&[(&then_value, then_block)]);
    //     if let Some(else_value) = else_value {
    //         phi_node.add_incoming(&[(&else_value.1, else_block)]);
    //     }
    //
    //     // then_type == else_value.0
    //     (then_type, phi_node.as_basic_value())
    // }
    //
    // /// Generates LLVM IR for struct literals.
    // fn generate_struct_literal(
    //     &mut self,
    //     struct_literal: &StructLiteral,
    // ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     let struct_literal_name = token_value!(&struct_literal.name);
    //
    //     let (struct_type, field_to_index) = {
    //         // TODO: Try to remove clone...
    //         let variable = self
    //             .symbol_table
    //             .get(&struct_literal_name)
    //             .expect("Unknown struct")
    //             .clone();
    //         match variable {
    //             Variable::StructDecl(struct_type, field_to_index) => (struct_type, field_to_index),
    //             _ => panic!("Not a struct"),
    //         }
    //     };
    //
    //     let twine = format!("struct_literal_{}", struct_literal_name);
    //     let struct_ptr = self.create_entry_block_alloca(struct_type, &twine);
    //
    //     for (field_name, field_expr) in struct_literal.fields.iter() {
    //         let field_name = token_value!(field_name);
    //         let (field_index, _) = field_to_index.get(&field_name).expect("Unknown field");
    //         let (_, field_expr_value) = self
    //             .generate_expression(field_expr)
    //             .expect("Failed to generate field expression");
    //
    //         let gep = self
    //             .builder
    //             .build_struct_gep(struct_type, struct_ptr, *field_index as u32, &field_name)
    //             .expect("Failed to build GEP");
    //         self.builder
    //             .build_store(gep, field_expr_value)
    //             .expect("Failed to store field");
    //     }
    //
    //     // Return the pointer to the struct
    //     let cart_type: CartType = struct_type.as_basic_type_enum().into();
    //     (
    //         cart_type.with_name(struct_literal_name).with_alloca(),
    //         struct_ptr.as_basic_value_enum(),
    //     )
    // }
    //
    // /// Generates LLVM IR for struct access expressions.
    // fn generate_struct_access(
    //     &mut self,
    //     struct_access: &StructAccessExpr,
    // ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     // Fields: struct_access_expr.fields. For example if there's a struct `User` assigned to
    //     // `user`, then `user.name.first` would be represented as [name, first].
    //     // Object: struct_access_expr.object. This is the struct object itself.
    //     // TODO: Support multiple fields
    //     let field_name = token_value!(&struct_access.fields[0]);
    //     let (cart_struct_type, struct_value_or_ptr) = self
    //         .generate_expression(&struct_access.object)
    //         .expect("Can't generate object");
    //
    //     let struct_object_name = cart_struct_type.name().expect("Can't retrieve name");
    //
    //     let struct_ptr = match cart_struct_type.is_alloca {
    //         true => struct_value_or_ptr.into_pointer_value(),
    //         false => {
    //             let struct_ptr =
    //                 self.create_entry_block_alloca(cart_struct_type.type_enum, struct_object_name);
    //             self.builder
    //                 .build_store(struct_ptr, struct_value_or_ptr)
    //                 .unwrap();
    //             struct_ptr
    //         }
    //     };
    //
    //     let (index, field_type) = match self
    //         .symbol_table
    //         .get(struct_object_name)
    //         .expect("Can't retrieve variable")
    //     {
    //         Variable::StructDecl(_, field_to_index) => {
    //             field_to_index.get(&field_name).expect("Unknown field")
    //         }
    //         _ => panic!("Not a struct"),
    //     };
    //
    //     let bte: BasicTypeEnum = cart_struct_type.into();
    //     let gep = self
    //         .builder
    //         .build_struct_gep(bte, struct_ptr, *index as u32, &field_name)
    //         .expect("Failed to build GEP");
    //
    //     (field_type.clone().with_alloca(), gep.as_basic_value_enum())
    // }
    //
    // /// Generates LLVM IR for assignment expressions.
    // fn generate_assignment(
    //     &mut self,
    //     assignment: &AssignmentExpr,
    // ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     let (value_type, value) = self.generate_expression(&assignment.r_value).unwrap();
    //
    //     let value = if value_type.is_alloca {
    //         self.builder
    //             .build_load(
    //                 value_type.type_enum,
    //                 value.into_pointer_value(),
    //                 "loaded_value",
    //             )
    //             .expect("Failed to load value")
    //     } else {
    //         value
    //     };
    //
    //     let (_, l_value_expr) = self
    //         .generate_expression(&assignment.l_value)
    //         .expect("Failed to generate l_value expression");
    //
    //     let l_value = l_value_expr.into_pointer_value();
    //
    //     self.builder
    //         .build_store(l_value, value)
    //         .expect("Failed to store value");
    //
    //     (value_type, value)
    // }
    //
    // /// Generates LLVM IR for method call expressions.
    // fn generate_method_call(
    //     &mut self,
    //     method_call: &MethodCallExpr,
    // ) -> Option<(CartType<'ctx>, BasicValueEnum<'ctx>)> {
    //     let (cart_ty, callee_expr_ptr) = self
    //         .generate_expression(&method_call.object)
    //         .expect("Callee expression not found");
    //
    //     let method_name_str = token_value!(&method_call.method_name);
    //     // TODO: Support multiple fields
    //     let function_name = format!(
    //         "{}-{}",
    //         cart_ty.name().expect("Invalid struct name"),
    //         method_name_str
    //     );
    //
    //     let callee = self
    //         .module
    //         .get_function(&function_name)
    //         .expect("Callee function not found");
    //
    //     // TODO: Support for static functions
    //     if callee.count_params() - 1 != method_call.arguments.len() as u32 {
    //         panic!("Incorrect # of arguments")
    //     }
    //
    //     // add support for non-self here
    //     let mut arguments = vec![callee_expr_ptr.into()];
    //     for arg in method_call.arguments.iter() {
    //         let (arg_type, arg_value) = self
    //             .generate_expression(arg)
    //             .expect("Failed to generate arg");
    //         if arg_type.is_alloca {
    //             // TODO: pass by reference for some types
    //             // load the value
    //             let loaded = self
    //                 .builder
    //                 .build_load(
    //                     arg_type.type_enum,
    //                     arg_value.into_pointer_value(),
    //                     "loaded_var",
    //                 )
    //                 .expect("Failed to load value");
    //             arguments.push(loaded.into());
    //         } else {
    //             arguments.push(arg_value.into());
    //         }
    //     }
    //
    //     let call_site = self
    //         .builder
    //         .build_call(callee, &arguments, "call")
    //         .expect("Failed to build call");
    //
    //     if let Some(return_type) = callee.get_type().get_return_type() {
    //         let return_type: BasicTypeEnum = return_type.as_basic_type_enum();
    //         let return_value = call_site
    //             .try_as_basic_value()
    //             .left()
    //             .expect("Return type unsupported yet");
    //         Some((return_type.into(), return_value))
    //     } else {
    //         None
    //     }
    // }
    //
    // /// Generates LLVM IR for array literals.
    // fn generate_array_literal(
    //     &mut self,
    //     expressions: &[Expr],
    // ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     let values = expressions
    //         .iter()
    //         .map(|expr| {
    //             let (ty, value) = self.generate_expression(expr).unwrap();
    //             if ty.is_alloca {
    //                 self.builder
    //                     .build_load(ty.type_enum, value.into_pointer_value(), "loaded_value")
    //                     .expect("Failed to load value")
    //             } else {
    //                 value
    //             }
    //         })
    //         .collect::<Vec<BasicValueEnum<'ctx>>>();
    //
    //     let create_array = self
    //         .module
    //         .get_function("create_array")
    //         .expect("create_array not found");
    //     let arr_ptr = self
    //         .builder
    //         .build_call(
    //             create_array,
    //             &[self
    //                 .context
    //                 .i32_type()
    //                 .const_int(values.len() as u64, false)
    //                 .into()],
    //             "array",
    //         )
    //         .expect("Failed to build call to create_array")
    //         .try_as_basic_value()
    //         .left()
    //         .unwrap();
    //
    //     let values_ptr = self
    //         .builder
    //         .build_array_malloc(
    //             self.context.i32_type(),
    //             self.context
    //                 .i32_type()
    //                 .const_int(values.len() as u64, false),
    //             "values",
    //         )
    //         .expect("Failed to build array malloc");
    //
    //     for (i, &value) in values.iter().enumerate() {
    //         let index = self.context.i32_type().const_int(i as u64, false);
    //         let element_ptr = unsafe {
    //             self.builder
    //                 .build_gep(self.context.i32_type(), values_ptr, &[index], "element_ptr")
    //                 .expect("Failed to build GEP")
    //         };
    //         self.builder.build_store(element_ptr, value).unwrap();
    //     }
    //
    //     let multiple_push_fn = self
    //         .module
    //         .get_function("push_to_array_multiple")
    //         .expect("push_to_array_multiple not found");
    //     self.builder
    //         .build_call(
    //             multiple_push_fn,
    //             &[
    //                 arr_ptr.into(),
    //                 values_ptr.into(),
    //                 self.context
    //                     .i32_type()
    //                     .const_int(values.len() as u64, false)
    //                     .into(),
    //             ],
    //             "call_push_multiple",
    //         )
    //         .expect("Failed to call dynamic_array_push_multiple");
    //
    //     // let arr_struct_type = self
    //     //     .context
    //     //     .struct_type(
    //     //         &[
    //     //             self.context.i32_type().into(),                        // ref_count
    //     //             self.context.i32_type().into(),                        // size
    //     //             self.context.i32_type().into(),                        // capacity
    //     //             self.context.ptr_type(AddressSpace::default()).into(), // elements
    //     //         ],
    //     //         false,
    //     //     )
    //     //     .as_basic_type_enum();
    //
    //     // TODO: duplicate alloca created in let
    //     (
    //         CartType::from(
    //             self.context
    //                 .ptr_type(AddressSpace::default())
    //                 .as_basic_type_enum(),
    //         ),
    //         arr_ptr,
    //     )
    //     // let array_type = values[0].get_type();
    //     // let array = CartArray::new(self.context, array_type.into());
    //     // let array_ptr = array.allocate_array(self.context, &self.builder, values.len() as u32);
    //     // for value in values.iter() {
    //     //     array.push_element(&self.builder, self.context, array_ptr, *value);
    //     // }
    //     //
    //     // let cart_ty: CartType = self
    //     //     .context
    //     //     .ptr_type(AddressSpace::default())
    //     //     .as_basic_type_enum()
    //     //     .into();
    //
    //     // (cart_ty.with_array(), array_ptr.as_basic_value_enum())
    // }
    //
    // /// Generates LLVM IR for array access expressions.
    // fn generate_array_access(
    //     &mut self,
    //     array_access: &ArrayAccessExpr,
    // ) -> (CartType<'ctx>, BasicValueEnum<'ctx>) {
    //     let (cart_ty, array_ptr) = self
    //         .generate_expression(&array_access.array)
    //         .expect("Can't generate array");
    //     let (_, index) = self
    //         .generate_expression(&array_access.index)
    //         .expect("Can't generate index");
    //
    //     let array_ptr = if cart_ty.is_alloca {
    //         self.builder
    //             .build_load(
    //                 cart_ty.type_enum,
    //                 array_ptr.into_pointer_value(),
    //                 "loaded_array",
    //             )
    //             .expect("Failed to load array")
    //     } else {
    //         array_ptr
    //     };
    //
    //     let arr_struct_type = self
    //         .context
    //         .struct_type(
    //             &[
    //                 self.context.i32_type().into(),                        // ref_count
    //                 self.context.i32_type().into(),                        // size
    //                 self.context.i32_type().into(),                        // capacity
    //                 self.context.ptr_type(AddressSpace::default()).into(), // elements
    //             ],
    //             false,
    //         )
    //         .as_basic_type_enum();
    //
    //     let elements_ptr_ptr = self
    //         .builder
    //         .build_struct_gep(
    //             arr_struct_type,
    //             array_ptr.into_pointer_value(),
    //             3,
    //             "elements",
    //         )
    //         .expect("Failed to build GEP");
    //
    //     let elements_ptr = self
    //         .builder
    //         .build_load(
    //             self.context.ptr_type(AddressSpace::default()),
    //             elements_ptr_ptr,
    //             "load_elements_ptr",
    //         )
    //         .expect("Failed to load elements ptr")
    //         .into_pointer_value();
    //
    //     let gep = unsafe {
    //         self.builder
    //             .build_gep(
    //                 // TODO: don't assume it's an i32 array
    //                 self.context.i32_type(),
    //                 elements_ptr,
    //                 &[index.into_int_value()],
    //                 "array_access",
    //             )
    //             .expect("Failed to build GEP")
    //     };
    //
    //     // TODO: different types of arrays
    //     // Assume it is an i32 array
    //     let element_type = self.context.i32_type().as_basic_type_enum();
    //     (
    //         CartType::from(element_type).with_alloca(),
    //         gep.as_basic_value_enum(),
    //     )
    // }
}
