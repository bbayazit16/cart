use crate::hir::Program;
use crate::hir::{Declaration, Function};

impl Program {
    pub fn get_all_high_level_functions(&self) -> Vec<&Function> {
        let mut functions = Vec::new();
        for decl in &self.declarations {
            match decl {
                Declaration::Function(f) => {
                    functions.push(f);
                }
                Declaration::Statement(_) => {}
                Declaration::Struct { .. } => {}
                Declaration::Extension { .. } => {}
            }
        }
        functions
    }
}
