//! This module is responsible for generating LLVM IR from the AST.
//!
//! `ast_codegen`  is responsible for generating LLVM IR from the AST.
//!
//! The `CodeGen` struct is the main struct that is used to generate the LLVM IR.
//!
//! This module additionally contains the symbol table and its implementation, as well
//! as the definition of a Variable for the codegen.
//! The symbol table is used to store variables and their values.

use crate::ast::Program;
use crate::codegen::symbol_table::SymbolTable;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

pub(crate) mod compiler;
mod declarations;
mod expressions;
mod statements;
mod symbol_table;
mod type_check;
mod types;

/// Codegen struct is responsible for generating LLVM IR from the AST.
/// The struct contains the context, module, builder, and the symbol table.
/// The symbol table is used to keep track of the variables in the current scope.
/// The CodeGen struct is created with a context, and the generate method is called
/// with the program AST to generate the LLVM IR.
pub(crate) struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbol_table: SymbolTable<'ctx>,
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
        let builder = context.create_builder();
        CodeGen {
            context,
            module,
            builder,
            symbol_table: SymbolTable::default(),
        }
    }

    /// Generate the LLVM IR from the program AST.
    /// Returns a reference to the module.
    pub(crate) fn generate(&mut self, program: &Program) -> &Module<'ctx> {
        for declaration in program.declarations.iter() {
            self.generate_declaration(declaration);
        }
        &self.module
    }
}
