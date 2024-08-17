//! This module defines the Abstract Syntax Tree (AST) of the language.
//!
//! The AST represents the structure of the source code in a tree format, and allows
//! LLVM codegen.
//!
//! Each of the structs defined in this module implements the display method, for
//! when debugging without as many details compared to Debug are required.
//!
//! Implements the Display trait for all AST nodes.
//!
//! Implements lowering, which expands the AST to prepare it for code generation.
//! The expansion includes things like desugaring, moving elif statements inside one else block,
//! removing return statements  and inserting implicit block returns, and creating an else
//! branch for every if expression. This part handles things like early returns from functions.
//!
//!
//! The module implements the `NotRecovered` trait for each of the AST nodes. The returned
//! value is used as a placeholder when the AST can't be recovered.
mod display;
pub(crate) mod lowering;
pub(crate) mod not_recovered;

use crate::token::Token;

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Block(Box<Block>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Literal(Literal),
    Variable(Token, Option<Type>),
    Assignment(Box<AssignmentExpr>),
    Call(Box<CallExpr>),
    StructAccess(Box<StructAccessExpr>),
    If(Box<IfExpr>),
    Match(Box<MatchExpr>),
    StructLiteral(Box<StructLiteral>),
    EnumValue(Box<EnumValue>),
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    FunctionDecl(Box<FunctionDecl>),
    StructDecl(Box<StructDecl>),
    EnumDecl(Box<EnumDecl>),
    ErrorDecl(Box<ErrorDecl>),
    ExtensionDecl(Box<ExtensionDecl>),
    StatementDecl(Box<Stmt>),
    #[allow(unused)]
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub declarations: Vec<Declaration>,
    pub return_expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    Number(Token),
    String(Token),
    Bool(Token),
    #[allow(unused)]
    None,
}

#[derive(Debug, Clone)]
pub(crate) struct AssignmentExpr {
    pub l_value: Box<Expr>,
    pub r_value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) struct CallExpr {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) struct StructAccessExpr {
    pub object: Expr,
    pub fields: Vec<Token>,
}

#[derive(Debug, Clone)]
pub(crate) struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Box<Block>,
    pub elif_branches: Vec<(Expr, Block)>,
    pub else_branch: Option<Box<Block>>,
}

#[derive(Debug, Clone)]
pub(crate) struct MatchExpr {
    pub value: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub(crate) struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) enum Pattern {
    Identifier(Token),
    Wildcard,
    EnumOrStructVariant(PatternType, Token, Vec<Token>),
}

#[derive(Debug, Clone)]
pub(crate) enum PatternType {
    Enum,
    Struct,
}

#[derive(Debug, Clone)]
pub(crate) struct StructLiteral {
    pub name: Token,
    pub fields: Vec<(Token, Expr)>,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumValue {
    pub name: Token,
    pub variant: Option<Token>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub(crate) struct ErrorValue {
    pub name: Token,
    pub fields: Vec<(Token, Expr)>,
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    Expression(Box<Expr>),
    Use(Vec<Token>),
    Return(Option<Expr>),
    Let(Box<LetStmt>),
    // Assign(Box<Assignment>),
    For(Box<ForStmt>),
    While(Box<WhileStmt>),
    DoWhile(Box<DoWhileStmt>),
}

#[derive(Debug, Clone)]
pub(crate) struct LetStmt {
    pub name: Token,
    pub is_mut: bool,
    pub set_type: Option<Type>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub(crate) struct Assignment {
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) struct ForStmt {
    pub iterator: Option<Token>,
    pub iterable: Expr,
    pub body: Box<Block>,
}

#[derive(Debug, Clone)]
pub(crate) struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Block>,
}

#[derive(Debug, Clone)]
pub(crate) struct DoWhileStmt {
    pub body: Box<Block>,
    pub condition: Expr,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionDecl {
    pub name: Token,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
    pub generic_params: Vec<Type>,
}

#[derive(Debug, Clone)]
pub(crate) struct Parameter {
    pub name: Token,
    pub param_type: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct StructDecl {
    pub name: Token,
    pub fields: Vec<StructField>,
    pub generic_params: Vec<Type>,
}

#[derive(Debug, Clone)]
pub(crate) struct StructField {
    pub name: Token,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumDecl {
    pub name: Token,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub(crate) enum EnumVariant {
    Simple(Token),
    Tuple(Token, Vec<Type>),
    Struct(Token, Vec<StructField>),
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorDecl {
    pub name: Token,
    pub variants: Vec<ErrorVariant>,
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorVariant {
    pub name: Token,
    pub fields: Vec<StructField>,
    pub message: Token,
}

#[derive(Debug, Clone)]
pub(crate) struct ExtensionDecl {
    pub name: Token,
    pub generic_params: Vec<Type>,
    pub functions: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
    Simple(Token),
    Empty,
    Generic(Token, Vec<Type>),
    #[allow(unused)]
    GenericDecl(Vec<Type>),
    #[allow(unused)]
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub declarations: Vec<Declaration>,
}
