use crate::codegen::cart_type::CartType;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Represents a variable in the codegen.
/// The variable can either be mutable or immutable.
#[derive(Debug, Clone)]
pub(super) enum Variable<'ctx> {
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
pub(crate) struct SymbolTable<T> {
    symbol_table: Vec<HashMap<String, T>>,
}

// Where T is an array, add a push method.
impl<T> SymbolTable<Vec<T>> {
    /// Add to the symbol table.
    /// The function is unchecked, and it assumes that there is
    /// already at least one scope in the symbol table.
    #[inline]
    pub(crate) fn push(&mut self, name: &str, variable: T) {
        self.symbol_table
            .last_mut()
            .unwrap()
            .entry(name.to_string())
            .or_default()
            .push(variable);
    }
}

impl<T> SymbolTable<T> {
    /// Create a new symbol table.
    pub(crate) fn new() -> Self {
        Self {
            // Global scope
            symbol_table: vec![HashMap::new()],
        }
    }
    /// Get from the symbol table.
    /// The function looks at scopes from the current scope to outer scopes,
    /// in order. If the variable is found, it returns the variable.
    #[inline]
    pub(crate) fn get(&self, name: &str) -> Option<&T> {
        self.symbol_table
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
    }
    
    /// Get a mutable reference from the symbol table.
    /// The function looks at scopes from the current scope to outer scopes,
    /// in order. If the variable is found, it returns the variable.
    #[inline]
    pub(crate) fn get_mut(&mut self, name: &str) -> Option<&mut T> {
        self.symbol_table
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(name))
    }

    /// Add to the symbol table.
    /// The function is unchecked, and it assumes that there is
    /// already at least one scope in the symbol table.
    #[inline]
    pub(crate) fn add(&mut self, name: String, variable: T) {
        self.symbol_table.last_mut().unwrap().insert(name, variable);
    }

    /// Start a new scope in the symbol table.
    #[inline]
    pub(crate) fn begin_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    /// End the current scope in the symbol table.
    /// The variables are lost after the scope is ended.
    #[inline]
    pub(crate) fn end_scope(&mut self) {
        self.symbol_table.pop();
    }
}

impl<T> Default for SymbolTable<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// SymbolSet represents the variables/types available in a scope.
/// It functions in a similar way to SymbolTable, but it utilizes
/// a HashSet in place of a HashMap.
#[derive(Debug, Clone)]
pub(crate) struct SymbolSet<T: Eq + Hash> {
    symbol_set: Vec<HashSet<T>>,
}

impl<T> SymbolSet<T>
where
    T: Eq + Hash,
{
    /// Create a new symbol set.
    pub(crate) fn new() -> Self {
        Self {
            symbol_set: vec![HashSet::new()],
        }
    }

    /// Check if the variable is contained in the symbol set.
    /// The function looks at scopes from the current scope to outer scopes,
    /// in order.
    #[inline]
    pub(crate) fn contains(&self, name: &T) -> bool {
        self.symbol_set
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    /// Add to the symbol set.
    /// The function is unchecked, and it assumes that there is
    /// already at least one scope in the symbol set.
    #[inline]
    pub(crate) fn add(&mut self, name: T) {
        self.symbol_set.last_mut().unwrap().insert(name);
    }

    /// Start a new scope in the symbol set.
    #[inline]
    pub(crate) fn begin_scope(&mut self) {
        self.symbol_set.push(HashSet::new());
    }

    /// End the current scope in the symbol set.
    /// The variables are lost after the scope is ended.
    #[inline]
    pub(crate) fn end_scope(&mut self) {
        self.symbol_set.pop();
    }
}

impl<T> Default for SymbolSet<T>
where
    T: Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}
