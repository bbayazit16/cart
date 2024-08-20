use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::StructType;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;
use std::hash::{DefaultHasher, Hash, Hasher};

/// CartString is a simple string type used in the Cart language.
/// CartString is heap-allocated, and contains a pointer to the string data.
///
/// # String Layout
/// - The first 32 bits indicate the reference count of the string.
/// - The next 32 bits indicate the length of the string.
/// - The last 8 bits indicate a pointer to the string data.
pub(crate) struct CartString<'ctx> {
    pub(crate) llvm_type: StructType<'ctx>,
}

impl<'ctx> CartString<'ctx> {
    /// Create a new CartString type.
    pub(crate) fn new(context: &'ctx Context) -> Self {
        let ptr_type = context.ptr_type(AddressSpace::default());
        let i32_type = context.i32_type();

        let llvm_type = context.struct_type(
            &[
                i32_type.into(), // Reference count
                i32_type.into(), // Length
                ptr_type.into(), // Pointer to string data
            ],
            false,
        );

        Self { llvm_type }
    }

    /// Allocate and return a new string on the heap.
    pub(crate) fn from_string<'s>(
        context: &'ctx Context,
        builder: &'s Builder<'ctx>,
        string: &'s str,
    ) -> PointerValue<'ctx> {
        let cart_string = Self::new(context);
        cart_string.allocate_string(context, builder, string)
    }

    /// Allocate a new string on the heap.
    /// The string is allocated with a reference count of 1.
    pub(crate) fn allocate_string<'s>(
        &self,
        context: &'ctx Context,
        builder: &'s Builder<'ctx>,
        string: &'s str,
    ) -> PointerValue<'ctx> {
        let llvm_string_length = context.i32_type().const_int(string.len() as u64, false);
        let string_hash = Self::hash_string(string);
        let char_arr = builder
            .build_array_malloc(
                context.i8_type(),
                llvm_string_length,
                format!("string_{:x}", string_hash).as_str(),
            )
            .expect("Could not allocate string");

        // Now, char_arr is a pointer to the allocated memory. Copy the string into it.
        for (i, byte) in string.bytes().enumerate() {
            let gep_idx = context.i32_type().const_int(i as u64, false);
            let byte_ptr = unsafe {
                builder
                    .build_gep(
                        context.i8_type(),                   // Points to a byte
                        char_arr,                            // The pointer to the allocated memory
                        &[gep_idx],                          // The index of the byte
                        format!("byte_{:x}", byte).as_str(), // name
                    )
                    .expect("Could not build GEP")
            };
            builder
                .build_store(byte_ptr, context.i8_type().const_int(byte as u64, false))
                .unwrap_or_else(|_| panic!("Could not store string byte {}", byte));
        }

        // Allocate the string struct
        // (i32, i32, i8*)
        // Reference count, length, pointer to string data
        let string_ptr = builder
            .build_malloc(self.llvm_type, "string")
            .expect("Failed to allocate string");

        // Storing reference count
        let ref_count_ptr = builder
            .build_struct_gep(
                self.llvm_type,                                  // points to a CartString struct
                string_ptr, // The pointer to the allocated memory
                0,          // The index of the reference count
                format!("ref_count_{:x}", string_hash).as_str(), // name
            )
            .expect("Could not build GEP");
        builder
            // ref count 1 because the string was just allocated
            .build_store(ref_count_ptr, context.i32_type().const_int(1, false))
            .expect("Could not store reference count");

        // Storing string length
        let length_ptr = builder
            .build_struct_gep(
                self.llvm_type,                               // points to a CartString struct
                string_ptr,                                   // The pointer to the allocated memory
                1,                                            // The index of the length
                format!("length_{:x}", string_hash).as_str(), // name
            )
            .expect("Could not build GEP");
        builder
            .build_store(length_ptr, llvm_string_length)
            .expect("Could not store string length");

        // Storing string data
        // The original string data was written to `char_arr` above.
        // This gets a pointer to the string's data field and stores
        // the pointer to the string data.
        let data_ptr = builder
            .build_struct_gep(
                self.llvm_type,                             // points to a CartString struct
                string_ptr,                                 // The pointer to the allocated memory
                2,                                          // The index of the data
                format!("data_{:x}", string_hash).as_str(), // name
            )
            .expect("Could not build GEP");
        builder
            .build_store(data_ptr, char_arr)
            .expect("Could not store string data");

        // Return the pointer to the string struct
        string_ptr
    }

    /// Hash and return a unique identifier for the string.
    pub(crate) fn hash_string(string: &str) -> u64 {
        let hasher = &mut DefaultHasher::new();
        string.hash(hasher);
        hasher.finish()
    }
}
