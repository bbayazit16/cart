//! Implements the Display trait for all AST nodes.
//! While writing a macro for this is possible, it can quickly get hard
//! due to the issue #35853 in the Rust compiler:
//! 
//! nested macros don't allow repetitions in binding patterns #35853.
//! 
//! For now, the display method was implemented
//! 
use std::fmt;
use crate::token::Token;
use super::ast::*;

fn display_vec<T: fmt::Display + 'static>(v: &[T], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[")?;
    for (i, item) in v.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        match item {
            item if std::any::TypeId::of::<T>() == std::any::TypeId::of::<(Token, Expr)>() => {
                let pair = unsafe { &*(item as *const T as *const TokenExprPair) };
                write!(f, "{}", pair)?;
            },
            item if std::any::TypeId::of::<T>() == std::any::TypeId::of::<(Expr, Block)>() => {
                let pair = unsafe { &*(item as *const T as *const ExprBlockPair) };
                write!(f, "{}", pair)?;
            },
            _ => write!(f, "{}", item)?,
        }
    }
    write!(f, "]")
}

fn display_option<T: fmt::Display>(o: &Option<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match o {
        Some(v) => write!(f, "Some({})", v),
        None => write!(f, "None"),
    }
}

struct TokenExprPair(Token, Expr);
struct ExprBlockPair(Expr, Block);

impl fmt::Display for TokenExprPair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl fmt::Display for ExprBlockPair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Block(block) => write!(f, "Block({})", block),
            Expr::Binary(binary) => write!(f, "Binary({})", binary),
            Expr::Unary(unary) => write!(f, "Unary({})", unary),
            Expr::Literal(literal) => write!(f, "Literal({})", literal),
            Expr::Variable(token) => write!(f, "Variable({})", token),
            Expr::Assignment(assign) => write!(f, "Assignment({})", assign),
            Expr::Call(call) => write!(f, "Call({})", call),
            Expr::StructAccess(access) => write!(f, "StructAccess({})", access),
            Expr::IfExpr(if_expr) => write!(f, "IfExpr({})", if_expr),
            Expr::MatchExpr(match_expr) => write!(f, "MatchExpr({})", match_expr),
            Expr::StructLiteral(struct_literal) => write!(f, "StructLiteral({})", struct_literal),
            Expr::EnumValue(enum_value) => write!(f, "EnumValue({})", enum_value),
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Declaration::FunctionDecl(func) => write!(f, "FunctionDecl({})", func),
            Declaration::StructDecl(struct_decl) => write!(f, "StructDecl({})", struct_decl),
            Declaration::EnumDecl(enum_decl) => write!(f, "EnumDecl({})", enum_decl),
            Declaration::ErrorDecl(error_decl) => write!(f, "ErrorDecl({})", error_decl),
            Declaration::ExtensionDecl(ext_decl) => write!(f, "ExtensionDecl({})", ext_decl),
            Declaration::StatementDecl(stmt) => write!(f, "StatementDecl({})", stmt),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Block(declarations=")?;
        display_vec(&self.declarations, f)?;
        write!(f, ", return_expr=")?;
        display_option(&self.return_expr.as_ref().map(|b| b.as_ref()), f)?;
        write!(f, ")")
    }
}

impl fmt::Display for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BinaryExpr(left={}, operator={}, right={})", self.left, self.operator, self.right)
    }
}

impl fmt::Display for UnaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UnaryExpr(operator={}, right={})", self.operator, self.right)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(t) => write!(f, "Number({})", t),
            Literal::String(t) => write!(f, "String({})", t),
            Literal::Bool(t) => write!(f, "Bool({})", t),
            Literal::None => write!(f, "None"),
        }
    }
}

impl fmt::Display for AssignmentExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AssignmentExpr(l_value={}, r_value={})", self.l_value, self.r_value)
    }
}

impl fmt::Display for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CallExpr(callee={}, arguments=", self.callee)?;
        display_vec(&self.arguments, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for StructAccessExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StructAccessExpr(object={}, fields=", self.object)?;
        display_vec(&self.fields, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for IfExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IfExpr(condition={}, then_branch={}, elif_branches=", self.condition, self.then_branch)?;
        let elif_branches: Vec<ExprBlockPair> = self.elif_branches.iter()
            .map(|(expr, block)| ExprBlockPair(expr.clone(), block.clone()))
            .collect();
        display_vec(&elif_branches, f)?;
        write!(f, ", else_branch=")?;
        display_option(&self.else_branch.as_ref().map(|b| b.as_ref()), f)?;
        write!(f, ")")
    }
}

impl fmt::Display for MatchExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MatchExpr(value={}, arms=", self.value)?;
        display_vec(&self.arms, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MatchArm(pattern={}, body={})", self.pattern, self.body)
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Identifier(token) => write!(f, "Identifier({})", token),
            Pattern::Wildcard => write!(f, "Wildcard"),
            Pattern::EnumOrStructVariant(pattern_type, token, tokens) => {
                write!(f, "EnumOrStructVariant({:?}, {}, ", pattern_type, token)?;
                display_vec(tokens, f)?;
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for StructLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StructLiteral(name={}, fields=", self.name)?;
        let fields: Vec<TokenExprPair> = self.fields.iter()
            .map(|(token, expr)| TokenExprPair(token.clone(), expr.clone()))
            .collect();
        display_vec(&fields, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for EnumValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EnumValue(name={}, variant=", self.name)?;
        display_option(&self.variant, f)?;
        write!(f, ", arguments=")?;
        display_vec(&self.arguments, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for ErrorValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ErrorValue(name={}, fields=", self.name)?;
        let fields: Vec<TokenExprPair> = self.fields.iter()
            .map(|(token, expr)| TokenExprPair(token.clone(), expr.clone()))
            .collect();
        display_vec(&fields, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "Expression({})", expr),
            Stmt::Use(tokens) => {
                write!(f, "Use(")?;
                display_vec(tokens, f)?;
                write!(f, ")")
            },
            Stmt::Return(expr) => {
                write!(f, "Return(")?;
                display_option(expr, f)?;
                write!(f, ")")
            },
            Stmt::Let(let_stmt) => write!(f, "Let({})", let_stmt),
            Stmt::Assign(assign) => write!(f, "Assign({})", assign),
            Stmt::For(for_stmt) => write!(f, "For({})", for_stmt),
            Stmt::While(while_stmt) => write!(f, "While({})", while_stmt),
            Stmt::DoWhile(do_while_stmt) => write!(f, "DoWhile({})", do_while_stmt),
        }
    }
}

impl fmt::Display for LetStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LetStmt(name={}, is_mut={}, set_type=", self.name, self.is_mut)?;
        display_option(&self.set_type, f)?;
        write!(f, ", expr={})", self.expr)
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Assignment(name={}, value={})", self.name, self.value)
    }
}

impl fmt::Display for ForStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ForStmt(iterator=")?;
        display_option(&self.iterator, f)?;
        write!(f, ", iterable={}, body={})", self.iterable, self.body)
    }
}

impl fmt::Display for WhileStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WhileStmt(condition={}, body={})", self.condition, self.body)
    }
}

impl fmt::Display for DoWhileStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DoWhileStmt(body={}, condition={})", self.body, self.condition)
    }
}

impl fmt::Display for FunctionDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionDecl(name={}, params=", self.name)?;
        display_vec(&self.params, f)?;
        write!(f, ", return_type={}, body={}, generic_params=", self.return_type, self.body)?;
        display_vec(&self.generic_params, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parameter(name={}, param_type={})", self.name, self.param_type)
    }
}

impl fmt::Display for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StructDecl(name={}, fields=", self.name)?;
        display_vec(&self.fields, f)?;
        write!(f, ", generic_params=")?;
        display_vec(&self.generic_params, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StructField(name={}, field_type={})", self.name, self.field_type)
    }
}

impl fmt::Display for EnumDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EnumDecl(name={}, variants=", self.name)?;
        display_vec(&self.variants, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EnumVariant::Simple(token) => write!(f, "Simple({})", token),
            EnumVariant::Tuple(token, types) => {
                write!(f, "Tuple({}, ", token)?;
                display_vec(types, f)?;
                write!(f, ")")
            },
            EnumVariant::Struct(token, fields) => {
                write!(f, "Struct({}, ", token)?;
                display_vec(fields, f)?;
                write!(f, ")")
            },
        }
    }
}

impl fmt::Display for ErrorDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ErrorDecl(name={}, variants=", self.name)?;
        display_vec(&self.variants, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for ErrorVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ErrorVariant(name={}, fields=", self.name)?;
        display_vec(&self.fields, f)?;
        write!(f, ", message={})", self.message)
    }
}

impl fmt::Display for ExtensionDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ExtensionDecl(name={}, generic_params=", self.name)?;
        display_vec(&self.generic_params, f)?;
        write!(f, ", functions=")?;
        display_vec(&self.functions, f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Simple(token) => write!(f, "Simple({})", token),
            Type::Empty => write!(f, "()"),
            Type::Generic(token, types) => {
                write!(f, "Generic({}, ", token)?;
                display_vec(types, f)?;
                write!(f, ")")
            },
            Type::GenericDecl(types) => {
                write!(f, "GenericDecl(")?;
                display_vec(types, f)?;
                write!(f, ")")
            },
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Program(declarations=")?;
        display_vec(&self.declarations, f)?;
        write!(f, ")")
    }
}