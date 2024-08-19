use crate::codegen::cart_type::CartType;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::HashMap;

/// Represents a variable in the codegen.
/// The variable can either be mutable or immutable.
/// - Mutable: The variable is stored as alloca in the stack.
/// - Immutable: The variable is stored as a value.
#[derive(Debug, Clone)]
pub(crate) enum Variable<'ctx> {
    Mutable(CartType<'ctx>, PointerValue<'ctx>),
    Immutable(CartType<'ctx>, PointerValue<'ctx>),
    ImmutableParam(CartType<'ctx>, BasicValueEnum<'ctx>),
    StructDecl(StructType<'ctx>, HashMap<String, (usize, CartType<'ctx>)>),
}

/// SymbolTable struct represents the symbol table in the codegen.
/// The symbol table is used to store variables and their values.
/// The internal state is just a vector of hashmaps, representing
/// the scopes in the codegen.
#[derive(Debug, Clone)]
pub(super) struct SymbolTable<'ctx> {
    symbol_table: Vec<HashMap<String, Variable<'ctx>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    /// Create a new symbol table.
    pub(super) fn new() -> Self {
        Self {
            // Global scope
            symbol_table: vec![HashMap::new()],
        }
    }
    /// Get a variable from the symbol table.
    /// The function looks at scopes from the current scope to outer scopes,
    /// in order. If the variable is found, it returns the variable.
    #[inline]
    pub(super) fn get_variable(&self, name: &str) -> Option<&Variable<'ctx>> {
        self.symbol_table
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
    }

    /// Add a variable to the symbol table.
    /// The function is unchecked, and it assumes that there is
    /// already at least one scope in the symbol table.
    #[inline]
    pub(super) fn add_variable(&mut self, name: String, variable: Variable<'ctx>) {
        self.symbol_table.last_mut().unwrap().insert(name, variable);
    }

    /// Start a new scope in the symbol table.
    #[inline]
    pub(super) fn start_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    /// End the current scope in the symbol table.
    /// The variables are lost after the scope is ended.
    #[inline]
    pub(super) fn end_scope(&mut self) {
        self.symbol_table.pop();
    }
}

impl Default for SymbolTable<'_> {
    fn default() -> Self {
        Self::new()
    }
}
