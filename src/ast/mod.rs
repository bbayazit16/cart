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

pub(crate) mod span;

use crate::context::Span;
use crate::token::Token;

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    FunctionDecl(FunctionDecl),
    StructDecl(StructDecl),
    #[allow(unused)]
    EnumDecl(EnumDecl),
    #[allow(unused)]
    ErrorDecl(ErrorDecl),
    #[allow(unused)]
    ExtensionDecl(ExtensionDecl),
    StatementDecl(Stmt),
    #[allow(unused)]
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    Expression(Expr),
    #[allow(unused)]
    Use(Vec<Token>),
    Return(Span, Option<Expr>),
    Let(LetStmt),
    #[allow(unused)]
    For(ForStmt),
    #[allow(unused)]
    While(WhileStmt),
    #[allow(unused)]
    DoWhile(DoWhileStmt),
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Block(Block),
    Binary(BinaryExpr),
    #[allow(unused)]
    Unary(UnaryExpr),
    Literal(Literal),
    #[allow(unused)]
    Variable(Token),
    #[allow(unused)]
    Assignment(AssignmentExpr),
    Call(CallExpr),
    StructAccess(StructAccessExpr),
    MethodCall(MethodCallExpr),
    If(IfExpr),
    #[allow(unused)]
    Match(MatchExpr),
    StructLiteral(StructLiteral),
    #[allow(unused)]
    EnumValue(EnumValue),
    ArrayLiteral(Span, Vec<Expr>),
    #[allow(unused)]
    ArrayAccess(ArrayAccessExpr),
    NotRecovered,
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    Integer(Token),
    Float(Token),
    String(Token),
    Bool(Token),
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
    Simple(Token),
    Empty(Span),
    #[allow(unused)]
    Generic(Span, Token, Vec<Type>),
}

#[derive(Debug, Clone)]
pub(crate) enum Pattern {
    #[allow(unused)]
    Identifier(Token),
    Wildcard(Span),
    #[allow(unused)]
    EnumOrStructVariant(Span, PatternType, Token, Vec<Token>),
}

#[derive(Debug, Clone)]
pub(crate) enum PatternType {
    Enum,
    Struct,
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
pub(crate) struct ArrayAccessExpr {
    pub array: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct MethodCallExpr {
    pub object: Box<Expr>,
    #[allow(unused)]
    pub fields: Vec<Token>,
    pub method_name: Token,
    pub arguments: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub declarations: Vec<Declaration>,
    pub return_expr: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct UnaryExpr {
    #[allow(unused)]
    pub operator: Token,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AssignmentExpr {
    pub l_value: Box<Expr>,
    pub r_value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct CallExpr {
    // TODO: Maybe? implement callee as an expr
    pub callee: Token,
    pub arguments: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct StructAccessExpr {
    pub object: Box<Expr>,
    pub fields: Vec<Token>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Block,
    pub elif_branches: Vec<(Expr, Block)>,
    pub else_branch: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct MatchExpr {
    pub value: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct StructLiteral {
    pub name: Token,
    pub fields: Vec<(Token, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumValue {
    #[allow(unused)]
    pub name: Token,
    #[allow(unused)]
    pub variant: Option<Token>,
    pub arguments: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub(crate) struct ErrorValue {
    pub name: Token,
    pub fields: Vec<(Token, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct LetStmt {
    pub name: Token,
    pub is_mut: bool,
    #[allow(unused)]
    pub set_type: Option<Type>,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct ForStmt {
    #[allow(unused)]
    pub iterator: Option<Token>,
    pub iterable: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct WhileStmt {
    pub condition: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct DoWhileStmt {
    pub body: Block,
    pub condition: Expr,
    pub span: Span,
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
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Parameter {
    pub name: Token,
    pub param_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct StructDecl {
    pub name: Token,
    pub fields: Vec<StructField>,
    #[allow(unused)]
    pub generic_params: Vec<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct StructField {
    pub name: Token,
    pub field_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumDecl {
    #[allow(unused)]
    pub name: Token,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorDecl {
    #[allow(unused)]
    pub name: Token,
    pub variants: Vec<ErrorVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorVariant {
    #[allow(unused)]
    pub name: Token,
    pub fields: Vec<StructField>,
    #[allow(unused)]
    pub message: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct ExtensionDecl {
    #[allow(unused)]
    pub name: Token,
    #[allow(unused)]
    pub generic_params: Vec<Type>,
    pub functions: Vec<FunctionDecl>,
    pub span: Span,
    pub header_span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub declarations: Vec<Declaration>,
}
