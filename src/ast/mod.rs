//! This module defines the Abstract Syntax Tree (AST) of the language.
//!
//! The AST represents the structure of the source code in a tree format, and allows
//! LLVM codegen.
//!
//! Implements lowering, which expands the AST to prepare it for code generation.
//! The expansion includes things like desugaring, moving elif statements inside one else block,
//! removing return statements  and inserting implicit block returns, and creating an else
//! branch for every if expression. This part handles things like early returns from functions.
//!
//! The module implements the `NotRecovered` trait for each of the AST nodes. The returned
//! value is used as a placeholder when the AST can't be recovered.

pub(crate) mod lowering;
pub(crate) mod not_recovered;

use crate::token::Token;

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Block(Box<Block>),
    Binary(Box<BinaryExpr>),
    #[allow(unused)]
    Unary(Box<UnaryExpr>),
    Literal(Literal),
    #[allow(unused)]
    Variable(Token, Option<Type>),
    #[allow(unused)]
    Assignment(Box<AssignmentExpr>),
    Call(Box<CallExpr>),
    StructAccess(Box<StructAccessExpr>),
    MethodCall(Box<MethodCallExpr>),
    If(Box<IfExpr>),
    #[allow(unused)]
    Match(Box<MatchExpr>),
    StructLiteral(Box<StructLiteral>),
    #[allow(unused)]
    EnumValue(Box<EnumValue>),
    ArrayLiteral(Vec<Expr>),
    #[allow(unused)]
    ArrayAccess(Box<ArrayAccessExpr>),
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    FunctionDecl(Box<FunctionDecl>),
    StructDecl(Box<StructDecl>),
    #[allow(unused)]
    EnumDecl(Box<EnumDecl>),
    #[allow(unused)]
    ErrorDecl(Box<ErrorDecl>),
    #[allow(unused)]
    ExtensionDecl(Box<ExtensionDecl>),
    StatementDecl(Box<Stmt>),
    #[allow(unused)]
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) struct ArrayAccessExpr {
    pub array: Expr,
    pub index: Expr,
}

#[derive(Debug, Clone)]
pub(crate) struct MethodCallExpr {
    pub object: Expr,
    #[allow(unused)]
    pub fields: Vec<Token>,
    pub method_name: Token,
    pub arguments: Vec<Expr>,
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
    #[allow(unused)]
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    Number(Token),
    String(Token),
    #[allow(unused)]
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
    #[allow(unused)]
    Identifier(Token),
    Wildcard,
    #[allow(unused)]
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
    #[allow(unused)]
    pub name: Token,
    #[allow(unused)]
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
    #[allow(unused)]
    Use(Vec<Token>),
    Return(Option<Expr>),
    Let(Box<LetStmt>),
    // Assign(Box<Assignment>),
    #[allow(unused)]
    For(Box<ForStmt>),
    #[allow(unused)]
    While(Box<WhileStmt>),
    #[allow(unused)]
    DoWhile(Box<DoWhileStmt>),
}

#[derive(Debug, Clone)]
pub(crate) struct LetStmt {
    pub name: Token,
    pub is_mut: bool,
    #[allow(unused)]
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
    #[allow(unused)]
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
    #[allow(unused)]
    pub generic_params: Vec<Type>,
    pub is_self: bool,
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
    #[allow(unused)]
    pub generic_params: Vec<Type>,
}

#[derive(Debug, Clone)]
pub(crate) struct StructField {
    pub name: Token,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumDecl {
    #[allow(unused)]
    pub name: Token,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub(crate) enum EnumVariant {
    Simple(Token),
    #[allow(unused)]
    Tuple(Token, Vec<Type>),
    #[allow(unused)]
    Struct(Token, Vec<StructField>),
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorDecl {
    #[allow(unused)]
    pub name: Token,
    pub variants: Vec<ErrorVariant>,
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorVariant {
    #[allow(unused)]
    pub name: Token,
    pub fields: Vec<StructField>,
    #[allow(unused)]
    pub message: Token,
}

#[derive(Debug, Clone)]
pub(crate) struct ExtensionDecl {
    #[allow(unused)]
    pub name: Token,
    #[allow(unused)]
    pub generic_params: Vec<Type>,
    pub functions: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
    Simple(Token),
    Empty,
    #[allow(unused)]
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
