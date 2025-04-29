use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{BinaryOp, Expression, Type};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, IntValue};
use inkwell::IntPredicate;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for binary expressions.
    ///
    /// Binary expressions always return an R-Value, as the result of the function
    /// is a value that can be used in other expressions. If it was an L-Value, it would
    /// be a reference to a memory location, which would not be useful in this context.
    pub(super) fn generate_binary_r_value(
        &mut self,
        left: &Expression,
        left_type: &Type,
        op: &BinaryOp,
        right: &Expression,
    ) -> Value<'ctx> {
        // Steps to generate a binary expression are as follows:
        //      1) Generate the left expression. If it is not an R-Value, load it.
        //      2) Generate the right expression. If it is not an R-Value, load it.
        //      3) Apply the binary operator to the left and right values.
        //      4) Return the resulting value, which is an R-Value.
        let left = self.generate_expression_r_value(left).unwrap();
        let right = self.generate_expression_r_value(right).unwrap();

        // let left = self.cast_to_r_value(left_expression);
        // let right = self.cast_to_r_value(right_expression);

        self.apply_binary_op(left, left_type, op, right)
    }

    /// Applies a binary operator to two values.
    /// The returning type is always an R-Value, as the result of the function
    /// is a value that can be used in other expressions.
    fn apply_binary_op(
        &self,
        left: Value<'ctx>,
        left_type: &Type,
        op: &BinaryOp,
        right: Value<'ctx>,
    ) -> Value<'ctx> {
        match (left_type, op) {
            // Integer Operations
            (Type::Int | Type::Int64 | Type::Int128 | Type::Int256, _) => self.apply_int_op(
                BasicValueEnum::from(left).into_int_value(),
                BasicValueEnum::from(right).into_int_value(),
                op,
            ),

            // String Operations
            (Type::String, BinaryOp::Add) => self.concatenate_strings(left, right),

            (Type::String, unsupported_op) => {
                unimplemented!("{:?} not supported for String", unsupported_op)
            }

            // Boolean Operations
            (Type::Bool, BinaryOp::And | BinaryOp::Or) => {
                self.apply_bool_op(BasicValueEnum::from(left), BasicValueEnum::from(right), op)
            }

            (Type::Bool, unsupported_op) => {
                unimplemented!("{:?} not supported for Bool", unsupported_op)
            }

            (unsupported_type, _) => unimplemented!("Unsupported type: {:?}", unsupported_type),
        }
    }

    /// Applies a boolean operator to two integer values. This always results in an R-Value.
    fn apply_int_op(
        &self,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
        op: &BinaryOp,
    ) -> Value<'ctx> {
        use BinaryOp::*;
        let result = match op {
            Add => self
                .builder
                .build_int_add(left, right, "add")
                .unwrap()
                .as_basic_value_enum(),
            Sub => self
                .builder
                .build_int_sub(left, right, "sub")
                .unwrap()
                .as_basic_value_enum(),
            Mul => self
                .builder
                .build_int_mul(left, right, "mul")
                .unwrap()
                .as_basic_value_enum(),
            Div => self
                .builder
                .build_int_unsigned_div(left, right, "div")
                .unwrap()
                .as_basic_value_enum(),
            Mod => self
                .builder
                .build_int_signed_rem(left, right, "srem")
                .unwrap()
                .as_basic_value_enum(),
            And => self
                .builder
                .build_and(left, right, "and")
                .unwrap()
                .as_basic_value_enum(),
            Or => self
                .builder
                .build_or(left, right, "or")
                .unwrap()
                .as_basic_value_enum(),
            Eq | Neq | Lt | Gt | Le | Ge => {
                let predicate = match op {
                    Eq => IntPredicate::EQ,
                    Neq => IntPredicate::NE,
                    Lt => IntPredicate::SLT,
                    Gt => IntPredicate::SGT,
                    Le => IntPredicate::SLE,
                    Ge => IntPredicate::SGE,
                    _ => unreachable!(),
                };
                self.builder
                    .build_int_compare(predicate, left, right, &format!("{:?}", op).to_lowercase())
                    .unwrap()
                    .as_basic_value_enum()
            }
        };

        Value::new(self.to_basic_type_enum(&Type::Int).unwrap(), result)
    }

    /// Applies a boolean operator to two boolean values. This always results in an R-Value.
    fn apply_bool_op(
        &self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        op: &BinaryOp,
    ) -> Value<'ctx> {
        let left_int = self
            .builder
            .build_int_z_extend(
                left.into_int_value(),
                self.context.i64_type(),
                "left_bool_to_int",
            )
            .unwrap();

        let right_int = self
            .builder
            .build_int_z_extend(
                right.into_int_value(),
                self.context.i64_type(),
                "right_bool_to_int",
            )
            .unwrap();

        let result_int = match op {
            BinaryOp::And => self
                .builder
                .build_and(left_int, right_int, "and_bool")
                .unwrap(),
            BinaryOp::Or => self
                .builder
                .build_or(left_int, right_int, "or_bool")
                .unwrap(),
            _ => unreachable!(),
        };

        let result_int = self
            .builder
            .build_int_truncate(
                result_int,
                self.context.bool_type(),
                &format!("{:?}_bool_to_bool", op).to_lowercase(),
            )
            .unwrap();

        Value::new(
            self.to_basic_type_enum(&Type::Bool).unwrap(),
            BasicValueEnum::from(result_int),
        )
    }

    /// Concatenates two strings, where both strings are R-Values.
    /// The function works as follows:
    ///    - Note: __concat_strings takes in two pointers to the strings and returns a pointer
    ///      to the concatenated string. As this function returns an R-Value, we must
    ///      load the result from the function call.
    ///   - Steps:
    ///     1) Cast the left and right values to L-Values. This involves storing them in an alloca.
    ///     2) Call the __concat_strings function.
    ///     3) Load the result from the function call.
    ///     4) Return the loaded value.
    fn concatenate_strings(
        &self,
        left: Value<'ctx>,
        right: Value<'ctx>,
    ) -> Value<'ctx> {
        // 1) Allocate for the left value.
        let left_alloca =
            self.create_entry_block_alloca(BasicTypeEnum::from(left), "alloca_left_string");
        self.builder
            .build_store(left_alloca, BasicValueEnum::from(left))
            .unwrap();

        // 1) Allocate for the right value.
        let right_alloca =
            self.create_entry_block_alloca(BasicTypeEnum::from(right), "alloca_right_string");
        self.builder
            .build_store(right_alloca, BasicValueEnum::from(right))
            .unwrap();

        // 2) Call the __concat_strings function.
        let std_concat_strings = self
            .module
            .get_function("__concat_strings")
            .expect("__concat_strings not included in STD builder");

        let args = [left_alloca.into(), right_alloca.into()];

        let call_site = self
            .builder
            .build_call(std_concat_strings, &args, "call_concat_strings")
            .unwrap();

        let ptr_result = call_site
            .try_as_basic_value()
            .unwrap_left()
            .into_pointer_value();

        // 3) Load the result from the function call.
        let loaded = self
            .builder
            .build_load(
                // String type
                self.to_basic_type_enum(&Type::String).unwrap(),
                ptr_result,
                "loaded_concat_strings",
            )
            .unwrap();

        // 4) Return the loaded value.
        Value::new(self.to_basic_type_enum(&Type::String).unwrap(), loaded)
    }
}
