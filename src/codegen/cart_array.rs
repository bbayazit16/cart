use crate::codegen::cart_type::CartType;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::AddressSpace;

/// `CartArray` is a simple array type used in Cart. By default,
/// it is heap allocated, and contains a pointer to the array data.
///
/// Each `CartArray` must have the same type for all elements.
///
/// # Array Layout
/// - The first 32 bits indicate the reference count of the array.
/// - The next 32 bits indicate the length of the array.
/// - The next 32 bits indicate the capacity of the array.
/// - The last 8 bits indicate a pointer to the array data.
pub(crate) struct CartArray<'ctx> {
    pub(crate) llvm_type: StructType<'ctx>,
    pub(crate) element_type: CartType<'ctx>,
}

impl<'ctx> CartArray<'ctx> {
    /// Create a new `CartArray` type.
    pub(crate) fn new(context: &'ctx Context, element_type: CartType<'ctx>) -> Self {
        let ptr_type = context.ptr_type(AddressSpace::default());
        let i32_type = context.i32_type();

        let llvm_type = context.struct_type(
            &[
                i32_type.into(), // Reference count
                i32_type.into(), // Length
                i32_type.into(), // Capacity
                ptr_type.into(), // Pointer to array data
            ],
            false,
        );

        Self {
            llvm_type,
            element_type,
        }
    }

    /// Allocate and return a new array on the heap.
    pub(crate) fn allocate_array<'s>(
        &self,
        context: &'ctx Context,
        builder: &'s Builder<'ctx>,
        initial_capacity: u32,
    ) -> PointerValue<'ctx> {
        let capacity_value = context.i32_type().const_int(initial_capacity as u64, false);

        let elements_ptr = builder
            .build_array_malloc(
                self.element_type.type_enum, // Element type
                capacity_value,              // Initial capacity
                "array_data",
            )
            .expect("Could not allocate array data");

        let array_ptr = builder
            .build_malloc(self.llvm_type, "array")
            .expect("Could not allocate array");

        // With ref count 1
        let ref_count_ptr = builder
            .build_struct_gep(self.llvm_type, array_ptr, 0, "array_ref_count_ptr")
            .expect("Could not get ref count pointer");
        builder
            .build_store(ref_count_ptr, context.i32_type().const_int(1, false))
            .expect("Could not store ref count");

        // With length 0
        let length_ptr = builder
            .build_struct_gep(self.llvm_type, array_ptr, 1, "array_length_ptr")
            .expect("Could not get length pointer");
        builder
            .build_store(length_ptr, context.i32_type().const_int(0, false))
            .expect("Could not store array length");

        // With capacity `initial_capacity`
        let capacity_ptr = builder
            .build_struct_gep(self.llvm_type, array_ptr, 2, "array_capacity_ptr")
            .expect("Could not get capacity pointer");
        builder
            .build_store(capacity_ptr, capacity_value)
            .expect("Could not store array capacity");

        // With pointer to array data
        let data_ptr = builder
            .build_struct_gep(self.llvm_type, array_ptr, 3, "array_data_ptr")
            .expect("Could not get data pointer");
        builder
            .build_store(data_ptr, elements_ptr)
            .expect("Could not store array data pointer");

        array_ptr
    }

    /// Push an element to the array.
    /// If the array is full, the array will be reallocated with double the capacity.
    pub fn push_element(
        &self,
        builder: &Builder<'ctx>,
        context: &'ctx Context,
        array_ptr: PointerValue<'ctx>,
        new_element: BasicValueEnum<'ctx>,
    ) {
        let length_ptr = builder
            .build_struct_gep(self.llvm_type, array_ptr, 1, "size_ptr")
            .expect("Failed to get size pointer");
        let length = builder
            .build_load(context.i32_type(), length_ptr, "size")
            .expect("Failed to load size")
            .into_int_value();

        let capacity_field_ptr = builder
            .build_struct_gep(self.llvm_type, array_ptr, 2, "capacity_ptr")
            .expect("Failed to get capacity pointer");
        let capacity = builder
            .build_load(context.i32_type(), capacity_field_ptr, "capacity")
            .expect("Failed to load capacity")
            .into_int_value();

        let needs_resize = builder
            .build_int_compare(inkwell::IntPredicate::EQ, length, capacity, "needs_resize")
            .expect("Failed to compare size and capacity");

        let resize_block = context.append_basic_block(
            builder.get_insert_block().unwrap().get_parent().unwrap(),
            "resize",
        );
        let continue_block = context.append_basic_block(
            builder.get_insert_block().unwrap().get_parent().unwrap(),
            "continue",
        );

        builder
            .build_conditional_branch(needs_resize, resize_block, continue_block)
            .expect("Failed to build conditional branch");

        // Resize block
        builder.position_at_end(resize_block);
        let new_capacity = builder
            .build_int_mul(
                capacity,
                context.i32_type().const_int(2, false),
                "new_capacity",
            )
            .expect("Failed to calculate new capacity");
        let new_elements_ptr = builder
            .build_array_malloc(self.element_type.type_enum, new_capacity, "new_elements")
            .expect("Failed to allocate memory for resized array");

        // Copy over every element from the previous pointer to the new pointer
        let loop_block = context.append_basic_block(
            builder.get_insert_block().unwrap().get_parent().unwrap(),
            "loop",
        );
        let loop_end_block = context.append_basic_block(
            builder.get_insert_block().unwrap().get_parent().unwrap(),
            "loop_end",
        );

        let loop_var_ptr = builder
            .build_alloca(context.i32_type(), "loop_var")
            .expect("Failed to allocate loop var");
        builder
            .build_store(loop_var_ptr, context.i32_type().const_int(0, false))
            .expect("Failed to store loop var");
        builder
            .build_unconditional_branch(loop_block)
            .expect("Failed to build branch");

        // ==== Inside the loop block ====
        builder.position_at_end(loop_block);
        // Load the loop variable
        let loop_var_val = builder
            .build_load(context.i32_type(), loop_var_ptr, "loop_var")
            .expect("Failed to load loop var")
            .into_int_value();

        // Compare with length
        let loop_cond = builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                loop_var_val,
                length,
                "loop_cond",
            )
            .expect("Failed to compare loop var and length");

        builder
            .build_conditional_branch(loop_cond, loop_block, loop_end_block)
            .expect("Failed to build conditional branch");

        let elements_field_ptr = builder
            .build_struct_gep(self.llvm_type, array_ptr, 3, "elements_ptr")
            .expect("Failed to get elements field pointer");
        
        let old_element_ptr = unsafe {
            builder
                .build_gep(
                    self.element_type.type_enum,
                    elements_field_ptr,
                    &[loop_var_val],
                    "old_element_ptr",
                )
                .expect("Failed to build GEP")
        };
        let new_element_ptr = unsafe {
            builder
                .build_gep(
                    self.element_type.type_enum,
                    new_elements_ptr,
                    &[loop_var_val],
                    "new_element_ptr",
                )
                .expect("Failed to build GEP")
        };

        let element = builder
            .build_load(self.element_type.type_enum, old_element_ptr, "element")
            .expect("Failed to load element");
        builder
            .build_store(new_element_ptr, element)
            .expect("Failed to store element");

        // Increment the loop variable
        let incremented_loop_var = builder
            .build_int_add(
                loop_var_val,
                context.i32_type().const_int(1, false),
                "incremented_loop_var",
            )
            .expect("Failed to increment loop var");
        builder
            .build_store(loop_var_ptr, incremented_loop_var)
            .expect("Failed to store loop var");

        builder
            .build_unconditional_branch(loop_block)
            .expect("Failed to build branch");
        // ==== End of loop body ====

        builder.position_at_end(loop_end_block);

        // Update the array struct with the new elements pointer and capacity
        builder
            .build_store(capacity_field_ptr, new_capacity)
            .expect("Failed to store new capacity");
        builder
            .build_store(elements_field_ptr, new_elements_ptr)
            .expect("Failed to store new elements pointer");

        // Capacity doesn't need increment / continue
        builder
            .build_unconditional_branch(continue_block)
            .expect("Failed to build branch");
        builder.position_at_end(continue_block);
        let element_ptr = unsafe {
            builder
                .build_gep(
                    self.element_type.type_enum,
                    elements_field_ptr,
                    &[length],
                    "element_ptr",
                )
                .expect("Failed to build GEP")
        };
        builder
            .build_store(element_ptr, new_element)
            .expect("Failed to store element");

        // Increment the size
        let new_length = builder.build_int_add(
            length,
            context.i32_type().const_int(1, false),
            "length_incr",
        ).expect("Failed to increment length");

        builder.build_store(length_ptr, new_length).expect("Failed to store new length");
    }
}
