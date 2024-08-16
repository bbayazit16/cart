//! This module defines the Abstract Syntax Tree (AST) of the language.
//! 
//! The AST represents the structure of the source code in a tree format, and allows 
//! LLVM codegen.
//! 
//! Each of the structs defined in this module implements the display method, for
//! when debugging without as many details compared to Debug are required.
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    Block(Box<Block>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Literal(Literal),
    Variable(Token, Option<Type>),
    Assignment(Box<AssignmentExpr>),
    Call(Box<CallExpr>),
    StructAccess(Box<StructAccessExpr>),
    IfExpr(Box<IfExpr>),
    MatchExpr(Box<MatchExpr>),
    StructLiteral(Box<StructLiteral>),
    EnumValue(Box<EnumValue>),
    NotRecovered,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    FunctionDecl(Box<FunctionDecl>),
    StructDecl(Box<StructDecl>),
    EnumDecl(Box<EnumDecl>),
    ErrorDecl(Box<ErrorDecl>),
    ExtensionDecl(Box<ExtensionDecl>),
    StatementDecl(Box<Stmt>),
    NotRecovered,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub declarations: Vec<Declaration>,
    pub return_expr: Option<Box<Expr>>
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(Token),
    String(Token),
    Bool(Token),
    None,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub l_value: Box<Expr>,
    pub r_value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructAccessExpr {
    pub object: Expr,
    pub fields: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Box<Block>,
    pub elif_branches: Vec<(Expr, Block)>,
    pub else_branch: Option<Box<Block>>,
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub value: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(Token),
    Wildcard,
    EnumOrStructVariant(PatternType, Token, Vec<Token>),
}

#[derive(Debug, Clone)]
pub enum PatternType {
    Enum,
    Struct
}

#[derive(Debug, Clone)]
pub struct StructLiteral {
    pub name: Token,
    pub fields: Vec<(Token, Expr)>,
}

#[derive(Debug, Clone)]
pub struct EnumValue {
    pub name: Token,
    pub variant: Option<Token>,
    pub arguments: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub struct ErrorValue {
    pub name: Token,
    pub fields: Vec<(Token, Expr)>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Box<Expr>),
    Use(Vec<Token>),
    Return(Option<Expr>),
    Let(Box<LetStmt>),
    Assign(Box<Assignment>),
    For(Box<ForStmt>),
    While(Box<WhileStmt>),
    DoWhile(Box<DoWhileStmt>),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: Token,
    pub is_mut: bool,
    pub set_type: Option<Type>,
    pub expr: Expr
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Token,
    pub value: Box<Expr>
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub iterator: Option<Token>,
    pub iterable: Expr,
    pub body: Box<Block>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Block>,
}

#[derive(Debug, Clone)]
pub struct DoWhileStmt {
    pub body: Box<Block>,
    pub condition: Expr,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Token,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
    pub generic_params: Vec<Type>
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Token,
    pub param_type: Type,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: Token,
    pub fields: Vec<StructField>,
    pub generic_params: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Token,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: Token,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Simple(Token),
    Tuple(Token, Vec<Type>),
    Struct(Token, Vec<StructField>),
}

#[derive(Debug, Clone)]
pub struct ErrorDecl {
    pub name: Token,
    pub variants: Vec<ErrorVariant>,
}

#[derive(Debug, Clone)]
pub struct ErrorVariant {
    pub name: Token,
    pub fields: Vec<StructField>,
    pub message: Token,
}

#[derive(Debug, Clone)]
pub struct ExtensionDecl {
    pub name: Token,
    pub generic_params: Vec<Type>,
    pub functions: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Simple(Token),
    Empty,
    Generic(Token, Vec<Type>),
    GenericDecl(Vec<Type>),
    NotRecovered,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}
