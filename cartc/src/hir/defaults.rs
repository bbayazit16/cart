use crate::codegen::symbol_table::SymbolTable;
use crate::hir::{FunctionSignature, Type};

/// Return the functions available in the HIR by default. This includes standard functions
/// that are globally available, such as `print`.
pub(crate) fn default_functions() -> SymbolTable<FunctionSignature> {
    let mut functions = SymbolTable::default();

    functions.add(
        "print_number".to_string(),
        FunctionSignature {
            original_name: "print_number".to_string(),
            mangled_name: "print_number".to_string(),
            params: vec![("number".into(), Type::Int)],
            return_type: Type::Unit,
            generic_declarations: Vec::new(),
            is_self: false,
        },
    );
    functions.add(
        "print_string".to_string(),
        FunctionSignature {
            original_name: "print_string".to_string(),
            mangled_name: "print_string".to_string(),
            params: vec![("string".into(), Type::String)],
            return_type: Type::Unit,
            generic_declarations: Vec::new(),
            is_self: false,
        },
    );

    functions
}
