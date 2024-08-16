//! Checks and validates for types, including inferring types
//! in let statements, verifying that if and let statement conditions are booleans,
//! and assigning types to function parameters.

use crate::ast::{Declaration, Expr, FunctionDecl, Program, Stmt, Type};
use crate::errors::CompileError;
use crate::reporter::Reporter;
use std::collections::HashMap;

/// TypeChecker struct resolves variable types, generics, and scoped variables,
/// and assigns them to the AST nodes. If an error occurs, the TypeChecker
/// reports to the reporter.
pub struct TypeChecker {
    reporter: Reporter,
    errors: Vec<CompileError>,
    variable_map: Vec<HashMap<String, Type>>,
}

// impl TypeChecker {
//     pub fn type_check(&mut self, ast: &Program) {
//         for declaration in ast.declarations.iter() {
//             if let Err(e) = self.type_check_declaration(declaration) {
//                 self.errors.push(e);
//             }
//         }
//     }
//
//     fn type_check_declaration(&mut self, node: &Declaration) -> Result<(), CompileError> {
//         match node {
//             Declaration::FunctionDecl(ref func) => {
//                 self.type_check_function(func)
//             }
//             Declaration::StructDecl(ref struct_decl) => {
//                 self.type_check_struct(struct_decl)
//             }
//             Declaration::EnumDecl(ref enum_decl) => {
//                 self.type_check_enum(enum_decl)
//             }
//             Declaration::ErrorDecl(ref error_decl) => {
//                 self.type_check_error(error_decl)
//             }
//             Declaration::ExtensionDecl(ref extension_decl) => {
//                 self.type_check_extension(extension_decl)
//             },
//             Declaration::StatementDecl(ref stmt) => self.type_check_stmt(stmt),
//             Declaration::NotRecovered => Ok(())
//         }
//     }
//
//     fn type_check_function(&mut self, node: &FunctionDecl) -> Result<(), CompileError> {
//         let return_type = &node.return_type;
//         todo!()
//     }
//
//     fn type_check_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
//         match stmt {
//             Stmt::Expression(ref expr) => {
//                 self.type_check_expr(expr)
//             }
//             Stmt::Use(_) => todo!(),
//             Stmt::Return(_) => todo!(),
//             Stmt::Let(_) => todo!(),
//             Stmt::Assign(_) => todo!(),
//             Stmt::For(_) => todo!(),
//             Stmt::While(_) => todo!(),
//             Stmt::DoWhile(_) => todo!()
//         }
//     }
//
//     fn type_check_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
//         match expr {
//             Expr::Block(ref block) => self.type_check_block(block),
//             _ => todo!()
//         }
//     }
//
//     fn type_check_block(&mut self, block: &Vec<Stmt>) -> Result<(), CompileError> {
//         for stmt in block.iter() {
//             if let Err(e) = self.type_check_stmt(stmt) {
//                 self.errors.push(e);
//             }
//         }
//         Ok(())
//     }
// }
