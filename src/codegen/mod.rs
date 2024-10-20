//! This module is responsible for generating LLVM IR from the HIR.
//! The `CodeGen` struct is the main struct that is used to generate the LLVM IR.
//!
//! At this point, the types must already be verified and the HIR is verbose enough
//! to generate the LLVM IR.
//!
//! The module provides the definition for symbol table, used to store variables
//! and their values during codegen.

use crate::codegen::symbol_table::SymbolTable;
use crate::codegen::value::Value;
use crate::hir;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::PointerValue;
use std::collections::HashMap;

// mod cart_string;
pub(crate) mod compiler;
mod declarations;
mod expressions;
mod statements;
pub(crate) mod symbol_table;
mod types;
mod value;

type StructDefinition<'ctx> = (
    StructType<'ctx>,
    HashMap<String, (usize, BasicTypeEnum<'ctx>)>,
);

/// Codegen struct is responsible for generating LLVM IR from the AST.
/// The struct contains the context, module, builder, and the symbol table.
/// The symbol table is used to keep track of the variables in the current scope.
/// The CodeGen struct is created with a context, and the generate method is called
/// with the program AST to generate the LLVM IR.
pub(crate) struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbol_table: SymbolTable<Value<'ctx>>,
    loaded_symbol_table: SymbolTable<Value<'ctx>>,
    struct_definition_table: SymbolTable<StructDefinition<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    /// Create a new CodeGen struct given context,
    /// optionally specifying the module name. If the module name
    /// isn't specified, the default name is "main".
    pub(crate) fn new<S: AsRef<str>>(context: &'ctx Context, module_name: Option<S>) -> Self {
        let module_name_str = module_name
            .as_ref()
            .map_or("main", |s| s.as_ref())
            .to_string();
        let module = context.create_module(&module_name_str);

        Self::add_std_functions(context, &module);

        let builder = context.create_builder();
        CodeGen {
            context,
            module,
            builder,
            symbol_table: SymbolTable::default(),
            loaded_symbol_table: SymbolTable::default(),
            struct_definition_table: SymbolTable::default(),
        }
    }

    /// Adds the standard library functions to the module.
    /// The standard library functions are defined in the `cart_std` module.
    fn add_std_functions(context: &'ctx Context, module: &Module<'ctx>) {
        module.add_function(
            "print_number",
            context
                .void_type()
                .fn_type(&[context.i32_type().into()], false),
            None,
        );
        // module.add_function(
        //     "print_string",
        //     context
        //         .void_type()
        //         .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
        //     None,
        // );
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
    }

    /// Generate the LLVM IR from the program AST.
    /// Returns a reference to the module.
    pub(crate) fn generate(&mut self, program: &hir::Program) -> &Module<'ctx> {
        for declaration in program.declarations.iter() {
            self.generate_declaration(declaration);
        }
        &self.module
    }

    /// Allocates an alloca at the entry block.
    pub(super) fn create_entry_block_alloca<T: BasicType<'ctx>>(
        &self,
        ty: T,
        name: &str,
    ) -> PointerValue<'ctx> {
        // https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl07.html
        // > mem2reg only looks for alloca instructions in the entry block of the function.
        // > Being in the entry block guarantees that the alloca is only executed once,
        // > which makes analysis simpler.
        let entry_block = self.builder.get_insert_block().unwrap();
        let function = entry_block.get_parent().unwrap();
        let entry_builder = self.context.create_builder();
        entry_builder.position_at_end(function.get_first_basic_block().unwrap());

        entry_builder
            .build_alloca(ty, name)
            .expect("Failed to allocate variable")
    }
}
