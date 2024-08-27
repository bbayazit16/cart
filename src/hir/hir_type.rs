use crate::token::TokenType;

/// The type of variable or expression in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Type {
    Int,
    Int64,
    Int128,
    Int256,
    Float,
    Float64,
    Bool,
    String,
    Unit,
    Array(Box<Type>),
    Struct(String),
    Enum(String),
    Error(String),
    // A generic like StructType<String, SomeType<OtherType>>
    // is stored as (omitting the box):
    // Generic(
    //     Struct("StructType"),
    //     [String, Generic(SomeType, [OtherType])]
    // )
    // It is guaranteed that the first element of the tuple is not a Generic.
    Generic(Box<Type>, Vec<Type>),
    // A generic type like <T> is stored as:
    // DeclaredGenericType("T")
    DeclaredGeneric(String),
}

impl Type {
    /// Return true if the type can be cast to `other`.
    pub(crate) fn can_be_cast_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int64) => true,
            (Type::Int, Type::Int128) => true,
            (Type::Int, Type::Int256) => true,
            (Type::Int64, Type::Int128) => true,
            (Type::Int64, Type::Int256) => true,
            (Type::Int128, Type::Int256) => true,
            (Type::Int, Type::Float) => true,
            (Type::Int64, Type::Float) => true,
            (Type::Int64, Type::Float64) => true,
            _ => self == other,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

impl BinaryOp {
    /// Given the left and right types, predict the resulting output type of the binary operation.
    /// For now, only integer types, booleans, and strings are supported.
    /// If the given left and right types do not produce a valid output type, return None.
    pub fn predict_output(&self, left: &Type, right: &Type) -> Option<Type> {
        // TODO: Support more types
        // - For all mentions of float and int interoperability, floats are only supported up to
        // 64 bits,  and can only be operated on with integers up to and including int64.
        // If the integer is `int`, then the maximum supported operation is int32.
        // - Floats and integers can be added, subtracted, multiplied, and divided,
        // and results in a float.
        // - Integers can be added, subtracted, multiplied, and divided, and results in
        // an integer, that is the largest of the two types.
        // - String concatenation is done via the + operator
        // - Modulus is only supported for integers, and only produces an integer
        // - Division always produces a float
        // - Boolean operators such as or, and, negation, etc. all produce a boolean,
        // and expect booleans on both the left and right type.
        // - Comparison operators such as ==, !=, <, >, <=, >= all produce a boolean,
        // and are only valid for integers and floats. Left and right do not have to
        // be of the same type. Strings cannot be compared.
        use BinaryOp::*;
        use Type::*;

        match self {
            Add | Sub | Mul => match (left, right) {
                // Integer operations result in the largest type
                (Int, Int) => Some(Int),
                (Int64, Int64) => Some(Int64),
                (Int128, Int128) => Some(Int128),
                (Int256, Int256) => Some(Int256),
                (Int, Int64) | (Int64, Int) => Some(Int64),
                (Int, Int128) | (Int128, Int) => Some(Int128),
                (Int, Int256) | (Int256, Int) => Some(Int256),
                (Int64, Int128) | (Int128, Int64) => Some(Int128),
                (Int64, Int256) | (Int256, Int64) => Some(Int256),
                (Int128, Int256) | (Int256, Int128) => Some(Int256),

                // Float operations with integers
                (Float, Int) | (Int, Float) => Some(Float),
                (Float64, Int) | (Int, Float64) => Some(Float64),
                (Float64, Int64) | (Int64, Float64) => Some(Float64),

                // Float operations with floats
                (Float, Float) => Some(Float),
                (Float64, Float64) => Some(Float64),

                // String concatenation
                (String, String) if *self == Add => Some(String),

                _ => None,
            },
            Div => match (left, right) {
                // Division always results in a float
                (Int, Int) | (Int64, Int) | (Int, Int64) | (Int64, Int64) => Some(Float64),
                (Float, Int) | (Int, Float) => Some(Float),
                (Float64, Int) | (Int, Float64) => Some(Float64),
                (Float, Float) => Some(Float),
                (Float64, Float64) => Some(Float64),
                _ => None,
            },
            Mod => match (left, right) {
                // Modulus only for integers
                (Int, Int) => Some(Int),
                (Int64, Int64) => Some(Int64),
                (Int128, Int128) => Some(Int128),
                (Int256, Int256) => Some(Int256),
                _ => None,
            },
            And | Or => match (left, right) {
                // Boolean operations
                (Bool, Bool) => Some(Bool),
                _ => None,
            },
            Eq | Neq | Lt | Gt | Le | Ge => match (left, right) {
                // Comparison operations for integers and floats
                (Int, Int)
                | (Int64, Int)
                | (Int, Int64)
                | (Int64, Int64)
                | (Int128, Int128)
                | (Int256, Int256)
                | (Float, Float)
                | (Float64, Float)
                | (Float, Float64)
                | (Float64, Float64) => Some(Bool),
                _ => None,
            },
        }
    }
}

impl From<&TokenType> for BinaryOp {
    fn from(value: &TokenType) -> Self {
        match value {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Sub,
            TokenType::Star => BinaryOp::Mul,
            TokenType::Slash => BinaryOp::Div,
            // TODO: Exp
            TokenType::Percent => BinaryOp::Mod,
            TokenType::Ampersand => BinaryOp::And,
            TokenType::Pipe => BinaryOp::Or,
            TokenType::EqualEqual => BinaryOp::Eq,
            TokenType::BangEqual => BinaryOp::Neq,
            TokenType::LeftAngle => BinaryOp::Lt,
            TokenType::RightAngle => BinaryOp::Gt,
            TokenType::LessEqual => BinaryOp::Le,
            TokenType::GreaterEqual => BinaryOp::Ge,
            other => unimplemented!("Unsupported binary operator: {:?}", other),
        }
    }
}

impl From<&TokenType> for UnaryOp {
    fn from(value: &TokenType) -> Self {
        match value {
            TokenType::Minus => UnaryOp::Neg,
            TokenType::Bang => UnaryOp::Not,
            other => unimplemented!("Unsupported unary operator: {:?}", other),
        }
    }
}
