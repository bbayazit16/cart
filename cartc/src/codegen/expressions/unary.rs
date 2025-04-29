use crate::codegen::value::{Value};
use crate::codegen::CodeGen;
use crate::hir::{Expression, Type, UnaryOp};
use inkwell::values::{BasicValueEnum, IntValue};

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for unary expressions.
    ///
    /// All generated values are R-Values, as the result of the function
    /// is a value that can be used in other expressions.
    pub(super) fn generate_unary_r_value(
        &mut self,
        expr: &Expression,
        op: &UnaryOp,
        ty: &Type,
    ) -> Value<'ctx> {
        let expression = self.generate_expression_r_value(expr).unwrap();
        let int_value = BasicValueEnum::from(expression).into_int_value();
        
        match ty {
            Type::Int | Type::Int64 | Type::Int128 | Type::Int256 | Type::Float | Type::Float64 => {
                self.generate_numeric_unary_op(int_value, op, ty)
            }
            Type::Bool => self.generate_boolean_unary_op(int_value, op, ty),
            _ => unreachable!("Unary operator not implemented for type {:?}", ty),
        }
    }

    /// Generate unary operations for numeric types.
    fn generate_numeric_unary_op(
        &self,
        int_value: IntValue<'ctx>,
        op: &UnaryOp,
        ty: &Type,
    ) -> Value<'ctx> {
        let res = match op {
            UnaryOp::Neg => self.builder.build_int_neg(int_value, "neg").unwrap(),
            UnaryOp::Not => panic!("Unary 'Not' is not implemented for numeric types"),
        };

        Value::new(
            self.to_basic_type_enum(ty).unwrap(),
            BasicValueEnum::from(res),
        )
    }

    /// Generate unary operations for boolean types.
    fn generate_boolean_unary_op(
        &self,
        int_value: IntValue<'ctx>,
        op: &UnaryOp,
        ty: &Type,
    ) -> Value<'ctx> {
        let extended_value = self
            .builder
            .build_int_z_extend(int_value, self.context.i64_type(), "bool_to_int")
            .unwrap();

        let res = match op {
            UnaryOp::Not => self.builder.build_not(extended_value, "not").unwrap(),
            UnaryOp::Neg => panic!("Unary 'Neg' is not implemented for booleans"),
        };

        let truncated_result = self
            .builder
            .build_int_truncate(res, self.context.bool_type(), "int_to_bool")
            .unwrap();

        Value::new(
            self.to_basic_type_enum(ty).unwrap(),
            BasicValueEnum::from(truncated_result),
        )
    }
}
