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
    /// If the requested variable can't be generated as r-value, return None.
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
            _ => None,
        }
        
    }
}
