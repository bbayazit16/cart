use crate::ast::*;
use crate::context::FilePointer;
use crate::token::Token;

pub(crate) trait NotRecovered {
    #[allow(unused)]
    fn not_recovered() -> Self;
    fn is_not_recovered(&self) -> bool;
}

impl NotRecovered for Expr {
    fn not_recovered() -> Self {
        Expr::NotRecovered
    }
    fn is_not_recovered(&self) -> bool {
        matches!(self, Expr::NotRecovered)
    }
}

impl NotRecovered for Declaration {
    fn not_recovered() -> Self {
        Declaration::NotRecovered
    }

    fn is_not_recovered(&self) -> bool {
        matches!(self, Declaration::NotRecovered)
    }
}

impl NotRecovered for Block {
    fn not_recovered() -> Self {
        Block {
            declarations: vec![Declaration::not_recovered()],
            return_expr: None,
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.declarations.iter().any(|decl| decl.is_not_recovered())
            || self
                .return_expr
                .as_ref()
                .map_or(false, |expr| expr.is_not_recovered())
    }
}

impl NotRecovered for BinaryExpr {
    fn not_recovered() -> Self {
        BinaryExpr {
            left: Box::new(Expr::not_recovered()),
            operator: Token::not_recovered(),
            right: Box::new(Expr::not_recovered()),
        }
    }
    fn is_not_recovered(&self) -> bool {
        self.left.is_not_recovered() || self.right.is_not_recovered()
    }
}

impl NotRecovered for UnaryExpr {
    fn not_recovered() -> Self {
        UnaryExpr {
            operator: Token::not_recovered(),
            right: Expr::not_recovered(),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.right.is_not_recovered()
    }
}

impl NotRecovered for Literal {
    fn not_recovered() -> Self {
        Literal::None
    }

    fn is_not_recovered(&self) -> bool {
        matches!(self, Literal::None)
    }
}

impl NotRecovered for AssignmentExpr {
    fn not_recovered() -> Self {
        AssignmentExpr {
            l_value: Box::new(Expr::not_recovered()),
            r_value: Box::new(Expr::not_recovered()),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.l_value.is_not_recovered() || self.r_value.is_not_recovered()
    }
}

impl NotRecovered for CallExpr {
    fn not_recovered() -> Self {
        CallExpr {
            callee: Expr::not_recovered(),
            arguments: vec![Expr::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.callee.is_not_recovered() || self.arguments.iter().any(|arg| arg.is_not_recovered())
    }
}

impl NotRecovered for StructAccessExpr {
    fn not_recovered() -> Self {
        StructAccessExpr {
            object: Expr::not_recovered(),
            fields: vec![Token::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.object.is_not_recovered()
    }
}

impl NotRecovered for IfExpr {
    fn not_recovered() -> Self {
        IfExpr {
            condition: Box::new(Expr::not_recovered()),
            then_branch: Box::new(Block::not_recovered()),
            elif_branches: vec![(Expr::not_recovered(), Block::not_recovered())],
            else_branch: None,
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.condition.is_not_recovered()
            || self.then_branch.is_not_recovered()
            || self
                .elif_branches
                .iter()
                .any(|(cond, block)| cond.is_not_recovered() || block.is_not_recovered())
            || self
                .else_branch
                .as_ref()
                .map_or(false, |block| block.is_not_recovered())
    }
}

impl NotRecovered for MatchExpr {
    fn not_recovered() -> Self {
        MatchExpr {
            value: Box::new(Expr::not_recovered()),
            arms: vec![MatchArm::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.value.is_not_recovered() || self.arms.iter().any(|arm| arm.is_not_recovered())
    }
}

impl NotRecovered for MatchArm {
    fn not_recovered() -> Self {
        MatchArm {
            pattern: Pattern::not_recovered(),
            body: Box::new(Expr::not_recovered()),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.pattern.is_not_recovered() || self.body.is_not_recovered()
    }
}

impl NotRecovered for Pattern {
    fn not_recovered() -> Self {
        Pattern::Wildcard
    }

    fn is_not_recovered(&self) -> bool {
        matches!(self, Pattern::Wildcard)
    }
}

impl NotRecovered for StructLiteral {
    fn not_recovered() -> Self {
        StructLiteral {
            name: Token::not_recovered(),
            fields: vec![(Token::not_recovered(), Expr::not_recovered())],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.fields.iter().any(|(_, expr)| expr.is_not_recovered())
    }
}

impl NotRecovered for EnumValue {
    fn not_recovered() -> Self {
        EnumValue {
            name: Token::not_recovered(),
            variant: None,
            arguments: vec![Expr::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.arguments.iter().any(|arg| arg.is_not_recovered())
    }
}

impl NotRecovered for ErrorValue {
    fn not_recovered() -> Self {
        ErrorValue {
            name: Token::not_recovered(),
            fields: vec![(Token::not_recovered(), Expr::not_recovered())],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.fields.iter().any(|(_, expr)| expr.is_not_recovered())
    }
}

impl NotRecovered for Stmt {
    fn not_recovered() -> Self {
        Stmt::Expression(Box::new(Expr::not_recovered()))
    }

    fn is_not_recovered(&self) -> bool {
        match self {
            Stmt::Expression(expr) => expr.is_not_recovered(),
            _ => false,
        }
    }
}

impl NotRecovered for LetStmt {
    fn not_recovered() -> Self {
        LetStmt {
            name: Token::not_recovered(),
            is_mut: false,
            set_type: None,
            expr: Expr::not_recovered(),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.expr.is_not_recovered()
    }
}

impl NotRecovered for Assignment {
    fn not_recovered() -> Self {
        Assignment {
            name: Token::not_recovered(),
            value: Box::new(Expr::not_recovered()),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.value.is_not_recovered()
    }
}

impl NotRecovered for ForStmt {
    fn not_recovered() -> Self {
        ForStmt {
            iterator: None,
            iterable: Expr::not_recovered(),
            body: Box::new(Block::not_recovered()),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.iterable.is_not_recovered() || self.body.is_not_recovered()
    }
}

impl NotRecovered for WhileStmt {
    fn not_recovered() -> Self {
        WhileStmt {
            condition: Expr::not_recovered(),
            body: Box::new(Block::not_recovered()),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.condition.is_not_recovered() || self.body.is_not_recovered()
    }
}

impl NotRecovered for DoWhileStmt {
    fn not_recovered() -> Self {
        DoWhileStmt {
            body: Box::new(Block::not_recovered()),
            condition: Expr::not_recovered(),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.body.is_not_recovered() || self.condition.is_not_recovered()
    }
}

impl NotRecovered for FunctionDecl {
    fn not_recovered() -> Self {
        FunctionDecl {
            name: Token::not_recovered(),
            params: vec![Parameter::not_recovered()],
            return_type: Type::not_recovered(),
            body: Block::not_recovered(),
            generic_params: vec![Type::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.params.iter().any(|param| param.is_not_recovered()) || self.body.is_not_recovered()
    }
}

impl NotRecovered for Parameter {
    fn not_recovered() -> Self {
        Parameter {
            name: Token::not_recovered(),
            param_type: Type::not_recovered(),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.param_type.is_not_recovered()
    }
}

impl NotRecovered for StructDecl {
    fn not_recovered() -> Self {
        StructDecl {
            name: Token::not_recovered(),
            fields: vec![StructField::not_recovered()],
            generic_params: vec![Type::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.fields.iter().any(|field| field.is_not_recovered())
    }
}

impl NotRecovered for StructField {
    fn not_recovered() -> Self {
        StructField {
            name: Token::not_recovered(),
            field_type: Type::not_recovered(),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.field_type.is_not_recovered()
    }
}

impl NotRecovered for EnumDecl {
    fn not_recovered() -> Self {
        EnumDecl {
            name: Token::not_recovered(),
            variants: vec![EnumVariant::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.variants
            .iter()
            .any(|variant| variant.is_not_recovered())
    }
}

impl NotRecovered for EnumVariant {
    fn not_recovered() -> Self {
        EnumVariant::Simple(Token::not_recovered())
    }

    fn is_not_recovered(&self) -> bool {
        matches!(self, EnumVariant::Simple(token) if token.is_not_recovered())
    }
}

impl NotRecovered for ErrorDecl {
    fn not_recovered() -> Self {
        ErrorDecl {
            name: Token::not_recovered(),
            variants: vec![ErrorVariant::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.variants
            .iter()
            .any(|variant| variant.is_not_recovered())
    }
}

impl NotRecovered for ErrorVariant {
    fn not_recovered() -> Self {
        ErrorVariant {
            name: Token::not_recovered(),
            fields: vec![StructField::not_recovered()],
            message: Token::not_recovered(),
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.fields.iter().any(|field| field.is_not_recovered())
    }
}

impl NotRecovered for ExtensionDecl {
    fn not_recovered() -> Self {
        ExtensionDecl {
            name: Token::not_recovered(),
            generic_params: vec![Type::not_recovered()],
            functions: vec![FunctionDecl::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.functions.iter().any(|func| func.is_not_recovered())
    }
}

impl NotRecovered for Type {
    fn not_recovered() -> Self {
        Type::NotRecovered
    }

    fn is_not_recovered(&self) -> bool {
        matches!(self, Type::NotRecovered)
    }
}

impl NotRecovered for Program {
    fn not_recovered() -> Self {
        Program {
            declarations: vec![Declaration::not_recovered()],
        }
    }

    fn is_not_recovered(&self) -> bool {
        self.declarations.iter().any(|decl| decl.is_not_recovered())
    }
}

impl NotRecovered for (Token, Expr) {
    fn not_recovered() -> Self {
        (Token::not_recovered(), Expr::not_recovered())
    }

    fn is_not_recovered(&self) -> bool {
        self.0.is_not_recovered() || self.1.is_not_recovered()
    }
}

impl NotRecovered for Token {
    fn not_recovered() -> Self {
        Token::Eof(FilePointer::default())
    }

    fn is_not_recovered(&self) -> bool {
        matches!(self, Token::Eof(e) if *e == FilePointer::default())
    }
}
