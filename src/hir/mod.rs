//! The High-level Intermediate Representation (HIR) is a representation of the program that is
//! easier to work with compared to the AST. The HIR is closer to the LLVM IR, and requires
//! all types to be resolved. The HIR is generated upon parsing the AST and checking
//! the types of the program.
//!
//! If a program can compile into HIR without any errors, there should theoretically be
//! no compile errors when generating the LLVM IR. Therefore, HIR has a bunch of
//! token information stripped from the AST.

use crate::codegen::symbol_table::{SymbolSet, SymbolTable};
use crate::errors::CompileError;
use crate::reporter::Reporter;
use std::collections::HashMap;
use crate::hir::hir_type::Type;

mod type_check;
mod errors;
mod display;
mod hir_type;

pub(crate) use hir_type::*;

/// TypeChecker struct resolves variable types, generics, and scoped variables,
/// and assigns them to the AST nodes. If an error occurs, the TypeChecker
/// reports to the reporter.
pub(crate) struct TypeChecker {
    reporter: Reporter,
    errors: Vec<CompileError>,
    types: SymbolTable<(Type, bool)>,
    generics_table: SymbolSet<String>,
    functions: SymbolTable<FunctionSignature>,
    struct_fields: SymbolTable<HashMap<String, Type>>,
    struct_methods: SymbolTable<HashMap<String, FunctionSignature>>,
    struct_generics: SymbolTable<Vec<String>>,
}

impl TypeChecker {
    /// Create a new TypeChecker struct with the given reporter.
    pub(crate) fn new(reporter: Reporter) -> Self {
        let mut functions = SymbolTable::default();
        //         module.add_function(
        //     "print_number",
        //     context
        //         .void_type()
        //         .fn_type(&[context.i32_type().into()], false),
        //     None,
        // );
        functions.add(
            "print_number".to_string(),
            FunctionSignature {
                name: "print_number".to_string(),
                params: vec![Type::Int],
                return_type: Type::Unit,
                generic_declarations: Vec::new(),
                is_self: false,
            },
        );
        
        Self {
            reporter,
            errors: Vec::new(),
            types: SymbolTable::default(),
            generics_table: SymbolSet::default(),
            functions,
            struct_fields: SymbolTable::default(),
            struct_methods: SymbolTable::default(),
            struct_generics: SymbolTable::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub(crate) declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    Statement(Statement),
    Function(Function),
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
        generic_declarations: Vec<Type>,
    },
    Extension {
        struct_name: String,
        generics: Vec<Type>,
        functions: Vec<Function>,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub(crate) signature: FunctionSignature,
    pub(crate) body: Block,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) name: String,
    pub(crate) params: Vec<Type>,
    pub(crate) return_type: Type,
    pub(crate) generic_declarations: Vec<Type>,
    pub(crate) is_self: bool,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Expression(Expression),
    Let {
        name: String,
        is_mut: bool,
        ty: Type,
        value: Expression,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) declarations: Vec<Declaration>,
    pub(crate) return_expr: Option<Expression>,
    pub(crate) return_type: Type,
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    Literal {
        value: String,
        ty: Type,
    },
    ArrayLiteral {
        elements: Vec<Expression>,
        element_type: Type,
        array_type: Type,
    },
    ArrayAccess {
        array: Box<Expression>,
        element_type: Type,
        index: Box<Expression>,
        index_type: Type,
    },
    Binary {
        left: Box<Expression>,
        left_type: Type,
        op: BinaryOp,
        right: Box<Expression>,
        right_type: Type,
        // Why this separate field is required: binary expressions such as division may produce
        // a different type than the operands.
        resulting_type: Type,
    },
    Unary {
        expr: Box<Expression>,
        ty: Type,
        // A unary expression will always have the same resulting type as the operand.
        op: UnaryOp,
    },
    Variable {
        name: String,
        is_mut: bool,
        ty: Type,
    },
    Call {
        // TODO: Box<Expression> to account for more complex calls.
        callee: String,
        arguments: Vec<Expression>,
        return_type: Type,
    },
    MethodCall {
        object: Box<Expression>,
        object_ty: Type,
        object_name: String,
        method: String,
        arguments: Vec<Expression>,
        method_return_type: Type,
    },
    StructLiteral {
        struct_name: String,
        struct_type: Type,
        fields: Vec<(String, Expression)>,
    },
    StructAccess {
        object: Box<Expression>,
        object_ty: Type,
        object_name: String,
        field: String,
        returned_field_type: Type,
    },
    Assignment {
        l_value: Box<Expression>,
        l_value_type: Type,
        r_value: Box<Expression>,
        r_value_type: Type,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Block>,
        elif_branches: Vec<(Expression, Block)>,
        else_branch: Option<Box<Block>>,
        ty: Type,
    },
    Block(Box<Block>),
}

impl Expression {
    /// Return the resulting type of the expression.
    ///
    /// Note about the use of clone: For `Type`, clone is a low-cost operation.
    /// Most `Type` instances are enums with no fields, meaning their bits are directly
    /// copied (acting like the Copy trait). For the rest of the fields that include
    /// a String, the clone operation's cost is negligible since it is highly likely
    /// that the String length isn't large. For example, the length of a struct name,
    /// or a custom type name is likely not to be large.
    pub(crate) fn resulting_type(&self) -> Type {
        match self {
            Expression::Literal { ty, .. } => ty,
            Expression::Binary {
                resulting_type: result_type,
                ..
            } => result_type,
            Expression::Unary { ty, .. } => ty,
            Expression::Variable { ty, .. } => ty,
            Expression::Call { return_type, .. } => return_type,
            Expression::MethodCall {
                method_return_type, ..
            } => method_return_type,
            Expression::StructLiteral { struct_type, .. } => struct_type,
            Expression::StructAccess {
                returned_field_type,
                ..
            } => returned_field_type,
            Expression::ArrayLiteral { array_type, .. } => array_type,
            Expression::ArrayAccess { element_type, .. } => element_type,
            Expression::Assignment { r_value_type, .. } => r_value_type,
            Expression::If { ty, .. } => ty,
            Expression::Block(block) => &block.return_type,
        }
        .clone()
    }
}
