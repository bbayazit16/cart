mod assignment;
mod binary_op;
mod blocks;
mod call;
mod conditional;
mod literals;
mod struct_access;
mod struct_literals;
mod unary;
mod variables;

use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::Expression;

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR expression and returns an r-value.
    /// If the requested variable can't be generated as r-value, panic (a compiler bug that should
    /// have been caught earlier).
    pub(super) fn generate_expression_r_value(&mut self, expr: &Expression) -> Option<Value<'ctx>> {
        match expr {
            Expression::Literal { ref value, ref ty } => Some(self.generate_literal_r_value(value, ty)),
            Expression::Binary {
                ref left,
                ref left_type,
                ref op,
                ref right,
                .. // right_type and resulting_type are unused
            } => {
                Some(self.generate_binary_r_value(left, left_type, op, right))
            },
            Expression::Unary {
                ref expr,
                ref op,
                ref ty,
            } => Some(self.generate_unary_r_value(expr, op, ty)),
            Expression::If {
                ref condition,
                ref then_branch,
                ref else_branch,
                ref ty,
            } => Some(self.generate_if_expr_r_value(condition, then_branch, else_branch, ty)?),
             Expression::Call {
                ref mangled_callee,
                ref arguments,
                ref return_type,
                .. // original_callee, associated_struct are unused
            } => Some(self.generate_call_expr_r_value(mangled_callee, arguments, return_type)?),
            Expression::Variable {
                ref name,
                // ty is unused
                ..
            } => Some(self.generate_variable_r_value(name)),
            Expression::Assignment { l_value, l_value_type, r_value,
                r_value_type // r_value_type is unused
            } => {
                Some(self.generate_assignment_r_value(l_value, l_value_type, r_value, r_value_type))
            },
             Expression::StructLiteral {
                ref struct_name,
                ref struct_type,
                ref fields,
            } => Some(self.generate_struct_literal_r_value(struct_name, struct_type, fields)),
            Expression::StructAccess {
                ref object,
                ref object_name,
                ref field,
                ref returned_field_type,
                .. // object_ty is unused
            } => Some(self.generate_struct_access_r_value(
                object,
                object_name,
                field,
                returned_field_type,
            )),
            e => {
                dbg!(&e);
                unimplemented!()
            }
        }
    }

    /// Generates LLVM IR expression and returns an r-value.
    /// If the requested variable can't be generated as r-value, panic (a compiler bug).
    pub(super) fn generate_expression_l_value(&mut self, expr: &Expression) -> Option<Value<'ctx>> {
        match expr {
            Expression::Variable {
                ref name,
                // ty is unused
                ..
            } => Some(self.generate_variable_l_value(name)),
            Expression::StructAccess {
                ref object,
                ref object_name,
                ref field,
                ref returned_field_type,
                .. // object_ty is unused
            } => Some(self.generate_struct_access_l_value(
                object,
                object_name,
                field,
                returned_field_type,
            )),
            e => {
                dbg!(&e);
                unimplemented!()
            }
        }
        
    }

    // pub(super) fn generate_expression(&mut self, expr: &Expression) -> Option<ValueState<'ctx>> {
    //     match expr {
    //         Expression::Block(ref block) => Some(ValueState::R(self.generate_block(block, Vec::new())?)),
    //         Expression::Literal { ref value, ref ty } => Some(ValueState::R(self.generate_literal(value, ty))),
    //         Expression::Unary {
    //             ref expr,
    //             ref op,
    //             ref ty,
    //         } => Some(ValueState::R(self.generate_unary(expr, op, ty))),
    //         Expression::Binary {
    //             ref left,
    //             ref left_type,
    //             ref op,
    //             ref right,
    //             .. // right_type and resulting_type are unused
    //         } => Some(ValueState::R(self.generate_binary(left, left_type, op, right))),
    //         Expression::Variable { ref name,
    //             // ty is unused
    //             ..  } => Some(ValueState::L(self.generate_variable(name))),
    //         Expression::Call {
    //             ref mangled_callee,
    //             ref arguments,
    //             ref return_type,
    //             .. // original_callee, associated_struct are unused
    //         } => Some(ValueState::R(self.generate_call_expr(mangled_callee, arguments, return_type)?)),
    //         Expression::If {
    //             ref condition,
    //             ref then_branch,
    //             ref else_branch,
    //             ref ty,
    //         } => Some(ValueState::R(self.generate_if_expr(condition, then_branch, else_branch, ty)?)),
    //         Expression::StructLiteral {
    //             ref struct_name,
    //             ref struct_type,
    //             ref fields,
    //         } => Some(ValueState::R(self.generate_struct_literal(struct_name, struct_type, fields))),
    //         Expression::StructAccess {
    //             ref object,
    //             ref object_name,
    //             ref field,
    //             ref returned_field_type,
    //             .. // object_ty is unused
    //         } => Some(ValueState::L(self.generate_struct_access(
    //             object,
    //             object_name,
    //             field,
    //             returned_field_type,
    //         ))),
    //         Expression::Assignment { l_value, l_value_type, r_value,
    //             r_value_type // r_value_type is unused
    //         } => {
    //             Some(ValueState::R(self.generate_assignment(l_value, l_value_type, r_value, r_value_type)))
    //         },
    //         e => {
    //             dbg!(&e);
    //             unimplemented!()
    //         }
    //     }
    // }
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
