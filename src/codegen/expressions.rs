use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{BinaryOp, Block, Expression, Type};
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};
use inkwell::AddressSpace;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for an expression.
    pub(super) fn generate_expression(&mut self, expr: &Expression) -> Option<Value<'ctx>> {
        match expr {
            Expression::Block(ref block) => self.generate_block(block, Vec::new()),
            Expression::Literal { ref value, ref ty } => Some(self.generate_literal(value, ty)),
            Expression::Binary {
                ref left,
                ref left_type,
                ref op,
                ref right,
                ref right_type,
                ref resulting_type,
            } => Some(self.generate_binary(left, left_type, op, right, right_type, resulting_type)),
            Expression::Variable { ref name, ref ty } => Some(self.generate_variable(name, ty)),
            Expression::Call {
                ref callee,
                ref arguments,
                ref return_type,
            } => self.generate_call_expr(callee, arguments, return_type),
            Expression::If {
                ref condition,
                ref then_branch,
                ref elif_branches,
                ref else_branch,
                ref ty,
            } => self.generate_if_expr(condition, then_branch, elif_branches, else_branch, ty),
            Expression::StructLiteral {
                ref struct_name,
                ref struct_type,
                ref fields,
            } => Some(self.generate_struct_literal(struct_name, struct_type, fields)),
            Expression::StructAccess {
                ref object,
                ref object_ty,
                ref object_name,
                ref field,
                ref returned_field_type,
            } => Some(self.generate_struct_access(
                object,
                object_ty,
                object_name,
                field,
                returned_field_type,
            )),
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
        {
            self.symbol_table.begin_scope();

            for (name, mut variable) in variables {
                // Create a new entry block alloca for the variable.
                // Add the alloca to the symbol table, which can then be loaded.
                self.as_r_value(&mut variable);
                let alloca = self.create_entry_block_alloca(
                    variable.type_enum,
                    format!("alloca_input_var_{}", name).as_str(),
                );
                self.builder
                    .build_store(alloca, variable.basic_value)
                    .unwrap();
                self.symbol_table.add(
                    name.to_string(),
                    Value::new(variable.type_enum, alloca.as_basic_value_enum()),
                );
            }

            for declaration in block.declarations.iter() {
                self.generate_declaration(declaration);
            }
        }

        let value = block.return_expr.as_ref().map(|expr_| {
            let mut v = self
                .generate_expression(expr_)
                .expect("Type error unverified by type checker: return expression has no value");
            self.as_r_value(&mut v);
            v
        });

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
            Type::String => self.generate_string_literal(value),
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
            self.to_basic_type_enum(ty).unwrap(),
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
            _ => unreachable!("Invalid boolean value. File a bug report - error in type checker"),
        }
    }

    /// Generates a string literal.
    fn generate_string_literal(&self, value: &str) -> BasicValueEnum<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();

        let string_length_llvm_int = i64_type.const_int(value.len() as u64, false);

        // CartString: { i32, i32, i8* }
        let cart_string_llvm_type = self.context.struct_type(
            &[
                i64_type.into(), // Reference count
                i64_type.into(), // Length
                ptr_type.into(), // Pointer to string data
            ],
            false,
        );

        // let cart_string_ptr = self.create_entry_block_alloca(
        //     cart_string_llvm_type,
        //     format!("cart_string_{}", value).as_str(),
        // );
        let cart_string_ptr = self.builder.build_malloc(
            cart_string_llvm_type,
            format!("cart_string_{}", value).as_str(),
        ).unwrap();

        // Initialize reference count to 1:
        let ref_count_gep = self
            .builder
            .build_struct_gep(
                cart_string_llvm_type,
                cart_string_ptr,
                0,
                format!("ref_count_gep_{}", value).as_str(),
            )
            .unwrap();
        self.builder
            .build_store(ref_count_gep, i64_type.const_int(1, false))
            .unwrap();

        // Initialize length:
        let length_gep = self
            .builder
            .build_struct_gep(
                cart_string_llvm_type,
                cart_string_ptr,
                1,
                format!("length_gep_{}", value).as_str(),
            )
            .unwrap();
        self.builder
            .build_store(length_gep, i64_type.const_int(value.len() as u64, false))
            .unwrap();

        // Initialize string data:
        let char_arr = self
            .builder
            .build_array_malloc(
                self.context.i8_type(),
                string_length_llvm_int,
                format!("bytes_array_{}", value).as_str(),
            )
            .unwrap();

        // Now, char_arr is a pointer to the allocated memory. Copy the string into it.
        for (i, byte) in value.bytes().enumerate() {
            let gep_idx = i64_type.const_int(i as u64, false);
            let byte_ptr = unsafe {
                self.builder
                    .build_gep(
                        self.context.i8_type(),                               // Points to a byte
                        char_arr,   // The pointer to the allocated memory
                        &[gep_idx], // The index of the byte
                        format!("byte_{}", byte as char).as_str(), // name
                    )
                    .expect("Could not build GEP")
            };
            self.builder
                .build_store(
                    byte_ptr,
                    self.context.i8_type().const_int(byte as u64, false),
                )
                .unwrap_or_else(|_| panic!("Could not store string byte {}", byte));
        }

        let data_gep = self
            .builder
            .build_struct_gep(cart_string_llvm_type, cart_string_ptr, 2, "data_gep")
            .unwrap();
        self.builder.build_store(data_gep, char_arr).unwrap();

        cart_string_ptr.as_basic_value_enum()
    }

    /// Generates the LLVM IR for binary expressions.
    fn generate_binary(
        &mut self,
        left: &Expression,
        left_type: &Type,
        op: &BinaryOp,
        right: &Expression,
        right_type: &Type,
        resulting_type: &Type,
    ) -> Value<'ctx> {
        let mut left = self.generate_expression(left).unwrap();
        let mut right = self.generate_expression(right).unwrap();

        self.as_r_value(&mut left);
        self.as_r_value(&mut right);

        self.apply_binary_op(&left, left_type, op, &right, right_type, resulting_type)
    }

    /// Applies a binary operator to two values.
    fn apply_binary_op(
        &self,
        left: &Value<'ctx>,
        left_type: &Type,
        op: &BinaryOp,
        right: &Value<'ctx>,
        right_type: &Type,
        resulting_type: &Type,
    ) -> Value<'ctx> {
        // TODO: Only supports integer types for now. Add more types in the future.
        // This involves a larger match expression.

        let res = match op {
            BinaryOp::Add => self
                .builder
                .build_int_add(
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "add",
                )
                .unwrap(),
            BinaryOp::Sub => self
                .builder
                .build_int_sub(
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "sub",
                )
                .unwrap(),
            BinaryOp::Mul => self
                .builder
                .build_int_mul(
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "mul",
                )
                .unwrap(),
            BinaryOp::Div => self
                .builder
                .build_int_unsigned_div(
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "div",
                )
                .unwrap(),
            BinaryOp::Mod => self
                .builder
                .build_int_signed_rem(
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "srem",
                )
                .unwrap(),
            BinaryOp::And => self
                .builder
                .build_and(
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "and",
                )
                .unwrap(),
            BinaryOp::Or => self
                .builder
                .build_or(
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "or",
                )
                .unwrap(),
            BinaryOp::Eq => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "eq",
                )
                .unwrap(),
            BinaryOp::Neq => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "ne",
                )
                .unwrap(),
            BinaryOp::Lt => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::SLT,
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "slt",
                )
                .unwrap(),
            BinaryOp::Gt => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::SGT,
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "sgt",
                )
                .unwrap(),
            BinaryOp::Le => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::SLE,
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "sle",
                )
                .unwrap(),
            BinaryOp::Ge => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::SGE,
                    left.basic_value.into_int_value(),
                    right.basic_value.into_int_value(),
                    "sge",
                )
                .unwrap(),
        };

        Value::new(
            self.to_basic_type_enum(left_type).unwrap(),
            res.as_basic_value_enum(),
        )
    }

    /// Generates LLVM IR for variable expressions.
    fn generate_variable(&mut self, name: &str, ty: &Type) -> Value<'ctx> {
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
            match self.loaded_symbol_table.get(name) {
                Some(var_loaded_value) => {
                    // TODO: Check if needs l_value casting
                    *var_loaded_value
                }
                None => {
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
                    let value = Value::new(var_alloca.type_enum, loaded.as_basic_value_enum());
                    self.loaded_symbol_table.add(name.to_string(), value);
                    value
                }
            }
        }
    }

    /// Generates LLVM IR for call expressions.
    fn generate_call_expr(
        &mut self,
        callee: &String,
        arguments: &[Expression],
        return_type: &Type,
    ) -> Option<Value<'ctx>> {
        let callee_fn = self.module.get_function(callee).unwrap();

        let args: Vec<BasicMetadataValueEnum> = arguments
            .iter()
            .map(|arg| {
                // self.generate_expression(arg).unwrap().1.into()
                let mut value = self.generate_expression(arg).unwrap();
                self.as_r_value(&mut value);
                value.basic_value.into()
            })
            .collect();

        let call_site = self
            .builder
            .build_call(callee_fn, &args, format!("call_{}", callee).as_str())
            .unwrap();

        match return_type {
            Type::Unit => None,
            _ => Some(Value::new(
                self.to_basic_type_enum(return_type).unwrap(),
                call_site.try_as_basic_value().unwrap_left(),
            )),
        }
    }

    /// Generates LLVM IR for if expressions.
    fn generate_if_expr(
        &mut self,
        condition: &Expression,
        then_branch: &Block,
        elif_branches: &[(Expression, Block)],
        else_branch: &Option<Box<Block>>,
        ty: &Type,
    ) -> Option<Value<'ctx>> {
        let function = self
            .builder
            .get_insert_block()
            .expect("No insertion block")
            .get_parent()
            .expect("No parent");

        let then_block = self.context.append_basic_block(function, "then");
        let else_block = self.context.append_basic_block(function, "else");
        let exit_block = self.context.append_basic_block(function, "exit");

        // TODO: Handle elif branches
        let mut condition = self.generate_expression(condition).unwrap();
        self.as_r_value(&mut condition);

        self.builder
            .build_conditional_branch(
                condition.basic_value.into_int_value(),
                then_block,
                else_block,
            )
            .unwrap();

        self.builder.position_at_end(then_block);
        // TODO: Then block may not exist
        let then_value = self.generate_block(then_branch, Vec::new()).unwrap();

        self.builder.build_unconditional_branch(exit_block).unwrap();
        self.builder.position_at_end(else_block);

        let else_value = if let Some(ref else_block) = else_branch {
            self.generate_block(else_block, Vec::new())
        } else {
            None
        };

        self.builder.build_unconditional_branch(exit_block).unwrap();
        self.builder.position_at_end(exit_block);

        let phi_node = self
            .builder
            .build_phi(self.context.i32_type(), "if_phi")
            .unwrap();

        phi_node.add_incoming(&[(&then_value.basic_value, then_block)]);
        if let Some(else_value) = else_value {
            phi_node.add_incoming(&[(&else_value.basic_value, else_block)]);
        }

        // then_type == else_value.0
        // (then_type, phi_node.as_basic_value())
        Some(Value::new(
            // ty.to_basic_type_enum(&self.context).unwrap(),
            then_value.type_enum,
            phi_node.as_basic_value(),
        ))
    }

    /// Generates LLVM IR for struct literals.
    fn generate_struct_literal(
        &mut self,
        struct_name: &str,
        struct_type: &Type,
        fields: &[(String, Expression)],
    ) -> Value<'ctx> {
        let llvm_struct_type = self
            .struct_definition_table
            .get(struct_name)
            .unwrap()
            .0
            .as_basic_type_enum();

        let struct_ptr = self.create_entry_block_alloca(
            llvm_struct_type,
            format!("struct_literal_ptr_{}", struct_name).as_str(),
        );

        for (field_name, field_expr) in fields {
            let struct_field_value = self.generate_expression(field_expr).unwrap();

            let gep = self
                .builder
                .build_struct_gep(
                    llvm_struct_type,
                    struct_ptr,
                    self.struct_field_index(struct_name, field_name) as u32,
                    field_name,
                )
                .unwrap();
            self.builder
                .build_store(gep, struct_field_value.basic_value)
                .unwrap();
        }

        Value::new(
            self.context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            struct_ptr.as_basic_value_enum(),
        )
    }

    /// Generates LLVM IR for struct access expressions.
    fn generate_struct_access(
        &mut self,
        object: &Expression,
        object_ty: &Type,
        object_name: &str,
        field: &str,
        returned_field_type: &Type,
    ) -> Value<'ctx> {
        let mut struct_object = self.generate_expression(object).unwrap();
        self.as_r_value(&mut struct_object);
        let struct_type = self.struct_definition_table.get(object_name).unwrap().0;

        let field_index = self.struct_field_index(object_name, field);

        let gep = self
            .builder
            .build_struct_gep(
                struct_type,
                struct_object.basic_value.into_pointer_value(),
                field_index as u32,
                field,
            )
            .unwrap();

        Value::new(
            self.to_basic_type_enum(returned_field_type).unwrap(),
            // self.context.ptr_type(inkwell::AddressSpace::default()).as_basic_type_enum(),
            gep.as_basic_value_enum(),
        )
        .as_l_value()
        // TODO: Support multiple fields
    }

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
