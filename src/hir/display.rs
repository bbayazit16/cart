use crate::hir::{BinaryOp, Type};
use std::fmt::{Display, Formatter};

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let converted = match self {
            Type::Int => "int".into(),
            Type::Int64 => "int64".into(),
            Type::Int128 => "int128".into(),
            Type::Int256 => "int256".into(),
            Type::Float => "float".into(),
            Type::Float64 => "float64".into(),
            Type::Bool => "bool".into(),
            Type::String => "string".into(),
            Type::Unit => "()".into(),
            Type::Array(inner_ty) => format!("{}[]", inner_ty),
            Type::Struct(name) => name.into(),
            Type::Enum(name) => name.into(),
            Type::Error(name) => name.into(),
            Type::Generic(outer_name, inner_types) => format!(
                "{}<{}>",
                outer_name,
                inner_types
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::DeclaredGeneric(name) => name.into(),
        };

        write!(f, "{}", converted)
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Mod => "%",
                BinaryOp::And => "&&",
                BinaryOp::Or => "||",
                BinaryOp::Eq => "==",
                BinaryOp::Neq => "!=",
                BinaryOp::Lt => "<",
                BinaryOp::Gt => ">",
                BinaryOp::Le => "<=",
                BinaryOp::Ge => ">=",
            }
        )
    }
}
