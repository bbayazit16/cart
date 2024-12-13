use crate::ast::{Declaration, Expr, Literal, Pattern, Stmt, Type};
use crate::context::Span;

impl Declaration {
    pub(crate) fn span(&self) -> Span {
        match self {
            Declaration::FunctionDecl(f) => f.span,
            Declaration::StructDecl(s) => s.span,
            Declaration::EnumDecl(e) => e.span,
            Declaration::ErrorDecl(e) => e.span,
            Declaration::ExtensionDecl(e) => e.span,
            Declaration::StatementDecl(s) => s.span(),
            Declaration::NotRecovered => todo!(),
        }
    }
}

impl Stmt {
    pub(crate) fn span(&self) -> Span {
        match self {
            Stmt::Expression(expr) => expr.span(),
            Stmt::Use(_) => todo!(),
            Stmt::Return(return_span, return_expr_span) => {
                if let Some(expr) = return_expr_span {
                    return_span.merge(&expr.span())
                } else {
                    *return_span
                }
            }
            Stmt::Let(let_stmt) => let_stmt.span,
            Stmt::For(_) => todo!(),
            Stmt::While(_) => todo!(),
            Stmt::DoWhile(_) => todo!(),
            Stmt::NotRecovered => todo!(),
        }
    }
}

impl Expr {
    pub(crate) fn span(&self) -> Span {
        match self {
            Expr::Block(block) => block.span,
            Expr::Binary(binary_expr) => binary_expr.span,
            Expr::Unary(u) => u.span,
            Expr::Literal(l) => l.span(),
            Expr::Variable(v) => v.span,
            Expr::Assignment(a) => a.span,
            Expr::Call(c) => c.span,
            Expr::StructAccess(s) => s.span,
            Expr::MethodCall(m) => m.span,
            Expr::If(i) => i.span,
            Expr::Match(_) => todo!(),
            Expr::StructLiteral(s) => s.span,
            Expr::EnumValue(_) => todo!(),
            Expr::ArrayLiteral(span, _) => *span,
            Expr::ArrayAccess(a) => a.span,
            Expr::NotRecovered => todo!(),
        }
    }
}

impl Literal {
    pub(crate) fn span(&self) -> Span {
        match self {
            Literal::Integer(tok) => tok.span,
            Literal::Float(tok) => tok.span,
            Literal::String(tok) => tok.span,
            Literal::Bool(tok) => tok.span,
        }
    }
}

impl Type {
    pub(crate) fn span(&self) -> Span {
        match self {
            Type::Simple(s) => s.span,
            Type::Empty(s) => *s,
            Type::Generic(s, _, _) => *s,
        }
    }
}

impl Pattern {
    pub(crate) fn span(&self) -> Span {
        match self {
            Pattern::Identifier(ident) => ident.span,
            Pattern::Wildcard(s) => *s,
            Pattern::EnumOrStructVariant(s, _, _, _) => *s,
        }
    }
}
