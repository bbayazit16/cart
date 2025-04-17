use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};

#[derive(Debug, Clone, Copy)]
pub(super) struct LValue;

#[derive(Debug, Clone, Copy)]
pub(super) struct RValue;

/// `ValueState` is a type that represents the state of the `Value`.
#[derive(Debug, Clone, Copy)]
pub(super) enum ValueState<'ctx> {
    L(Value<'ctx, LValue>),
    R(Value<'ctx, RValue>),
}

/// `Value` wraps around Inkwell `BasicTypeEnum` and allows extra information
/// to be assigned to the types. It employs a builder-like pattern.
/// `Value` struct uses type-state pattern to enforce the correct usage of either
/// the l-value or r-value.
///
/// Whether a value is an R-Value or an L-Value is not simply determined by the type of
/// the value - i.e. it is possible for an alloca to be an R-Value, especially if it is
/// a pass-by-reference type such as a struct or an array. Then, the alloca's L-Value would
/// be another alloca that is a pointer to the original alloca. All L-Values are
/// allocas, but not all allocas are R-Values.
///
/// Regardless of whether a value is an R-Value or an L-Value, the `type_enum` field
/// always stores the type of the original value. So, an integer that is an R-Value will
/// have an integer type stored in `type_enum`, and NOT a pointer type.
///
/// Users must be mindful of this distinction when using the `Value` struct.
#[derive(Debug, Clone, Copy)]
pub(super) struct Value<'ctx, V> {
    type_enum: BasicTypeEnum<'ctx>,
    basic_value: BasicValueEnum<'ctx>,
    state: std::marker::PhantomData<V>,
}

// Only exists on the R-Value variant to prevent accidents. An L-Value still provides
// the into_pointer_value method.
// It is completely possible to convert to a PointerValue and then back to a BasicValueEnum,
// but forbidding direct conversion makes this more clear.
impl<'ctx> From<Value<'ctx, RValue>> for BasicValueEnum<'ctx> {
    fn from(value: Value<'ctx, RValue>) -> Self {
        value.basic_value
    }
}

impl<'ctx, V> From<Value<'ctx, V>> for BasicTypeEnum<'ctx> {
    fn from(value: Value<'ctx, V>) -> Self {
        value.type_enum
    }
}

impl<'ctx> From<Value<'ctx, LValue>> for PointerValue<'ctx> {
    fn from(value: Value<'ctx, LValue>) -> Self {
        value.basic_value.into_pointer_value()
    }
}

impl<'ctx> Value<'ctx, LValue> {
    /// Creates a new `Value` as an L-value.
    pub(super) fn new_l(type_enum: BasicTypeEnum<'ctx>, basic_value: BasicValueEnum<'ctx>) -> Self {
        assert!(matches!(basic_value, BasicValueEnum::PointerValue(_)));
        Self {
            type_enum,
            basic_value,
            state: std::marker::PhantomData,
        }
    }
}

impl<'ctx> Value<'ctx, RValue> {
    /// Creates a new `Value` as an R-value.
    pub(super) fn new_r(type_enum: BasicTypeEnum<'ctx>, basic_value: BasicValueEnum<'ctx>) -> Self {
        Self {
            type_enum,
            basic_value,
            state: std::marker::PhantomData,
        }
    }
}
