use crate::codegen::value::{RValue, Value, ValueState};
use crate::codegen::CodeGen;
use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;

impl<'ctx> CodeGen<'ctx> {
    /// Casts an LValue to an RValue, loading the value if necessary.
    /// If the value is already an RValue, it is returned as is.
    pub(super) fn cast_to_r_value(&mut self, value: ValueState<'ctx>) -> Value<'ctx, RValue> {
        match value {
            ValueState::L(l_value) => {
                let loaded = self
                    .builder
                    .build_load(
                        BasicTypeEnum::from(l_value),
                        PointerValue::from(l_value),
                        "cast_to_r_value",
                    )
                    .unwrap();
                Value::new_r(BasicTypeEnum::from(l_value), loaded)
            }
            ValueState::R(r_value) => r_value,
        }
    }
}
