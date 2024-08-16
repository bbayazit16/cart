//! This module defines the core AST, the desugared and
//! simplified version of the AST. The core AST is used
//! in the codegen.
#[derive(Debug)]
pub enum Declaration {
    Function { 
        name: String,
        params: Vec<Parameter>
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug)]
pub struct Type {}