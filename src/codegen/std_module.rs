use inkwell::context::Context;
use inkwell::module::Module;

/// Builder for the standard library module.
pub struct StdModuleBuilder<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
}

impl<'ctx> StdModuleBuilder<'ctx> {
    /// Creates a new `StdModuleBuilder` with the given context and module.
    pub fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
        Self { context, module }
    }

    /// Consumes the builder and returns the module.
    pub fn build(self) -> Module<'ctx> {
        self.module
    }

    /// Adds the standard print functions to the module.
    pub fn add_print(self) -> Self {
        self.module.add_function(
            "print_number",
            self.context
                .void_type()
                .fn_type(&[self.context.i32_type().into()], false),
            None,
        );

        self.module.add_function(
            "print_string",
            self.context.void_type().fn_type(
                &[self
                    .context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into()],
                false,
            ),
            None,
        );

        self
    }

    /// Adds standard String functions to the module.
    pub fn add_string(self) -> Self {
        self.module.add_function(
            "__concat_strings",
            self.context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(
                    &[
                        self.context
                            .ptr_type(inkwell::AddressSpace::default())
                            .into(),
                        self.context
                            .ptr_type(inkwell::AddressSpace::default())
                            .into(),
                    ],
                    false,
                ),
            None,
        );

        self
    }
}

// module.add_function(
//     "create_array",
//     context
//         .ptr_type(AddressSpace::default())
//         .fn_type(&[context.i32_type().into()], false),
//     None,
// );
// module.add_function(
//     "push_to_array",
//     context.void_type().fn_type(
//         &[
//             context.ptr_type(AddressSpace::default()).into(),
//             context.i32_type().into(),
//         ],
//         false,
//     ),
//     None,
// );
// module.add_function(
//     "push_to_array_multiple",
//     context.void_type().fn_type(
//         &[
//             context.ptr_type(AddressSpace::default()).into(),
//             context.i32_type().into(),
//         ],
//         false,
//     ),
//     None,
// );
