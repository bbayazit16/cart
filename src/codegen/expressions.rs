use crate::ast::{Block, CallExpr, Expr, IfExpr, Literal};
use crate::codegen::symbol_table::Variable;
use crate::codegen::CodeGen;
use crate::token::Token;
use crate::token_value;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, IntValue};
use inkwell::IntPredicate;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for an expression.
    pub(super) fn generate_expression(&mut self, expr: &Expr) -> Option<BasicValueEnum<'ctx>> {
        // dbg!(&expr);
        match expr {
            Expr::Block(ref block) => self.generate_block(block, Vec::new()),
            Expr::Literal(ref literal) => self.generate_literal(literal),
            Expr::Binary(ref binary_expr) => Some(self.generate_binary(binary_expr)),
            Expr::Variable(ref token, ..) => Some(self.generate_variable(token)),
            Expr::Call(ref call_expr) => self.generate_call_expr(call_expr),
            Expr::If(ref if_expr) => self.generate_if_expr(if_expr),
            _ => todo!(),
        }
    }

    /// Generates the LLVM IR for a block.
    /// Optionally, pass a tuple of values that will be added as variables
    /// to the symbol table after starting the scope.
    pub(super) fn generate_block(
        &mut self,
        block: &Block,
        variables: Vec<(String, Variable<'ctx>)>,
    ) -> Option<BasicValueEnum<'ctx>> {
        {
            self.symbol_table.start_scope();

            for (name, variable) in variables {
                self.symbol_table.add_variable(name, variable);
            }

            for declaration in block.declarations.iter() {
                self.generate_declaration(declaration);
            }
        }

        let return_value = if let Some(return_expr) = &block.return_expr {
            self.generate_expression(return_expr)
        } else {
            None
        };

        self.symbol_table.end_scope();

        return_value
    }

    /// Generates the LLVM IR for a literal.
    fn generate_literal(&self, literal: &Literal) -> Option<BasicValueEnum<'ctx>> {
        match literal {
            Literal::Number(token) => {
                let value = token_value!(token).parse::<i32>().unwrap();
                Some(
                    self.context
                        .i32_type()
                        .const_int(value as u64, false)
                        .into(),
                )
            }
            _ => unimplemented!("Other literal types"),
        }
    }

    /// Generates the LLVM IR for binary expressions.
    fn generate_binary(&mut self, binary_expr: &crate::ast::BinaryExpr) -> BasicValueEnum<'ctx> {
        let left = self.generate_expression(&binary_expr.left).unwrap();
        let right = self.generate_expression(&binary_expr.right).unwrap();
        let operator = &binary_expr.operator;

        if left.get_type() != right.get_type() {
            panic!("Binary expression types do not match");
        }

        match left.get_type() {
            BasicTypeEnum::IntType(_) => {
                let left = left.into_int_value();
                let right = right.into_int_value();
                self.generate_int_binary(left, right, operator)
                    .as_basic_value_enum()
            }
            _ => unimplemented!("Binary expressions for non-int types"),
        }
    }

    /// Generates LLVM IR for integer binary expressions.
    fn generate_int_binary(
        &mut self,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
        operator: &Token,
    ) -> IntValue<'ctx> {
        match operator {
            Token::Plus(..) => self.builder.build_int_add(left, right, "add").unwrap(),
            Token::Minus(..) => self.builder.build_int_sub(left, right, "sub").unwrap(),
            Token::Star(..) => self.builder.build_int_mul(left, right, "mul").unwrap(),
            Token::Slash(..) => self
                .builder
                .build_int_unsigned_div(left, right, "div")
                .unwrap(),
            Token::EqualEqual(..) => self
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "eq")
                .unwrap(),
            Token::LessEqual(..) => self
                .builder
                .build_int_compare(IntPredicate::SLE, left, right, "sle")
                .unwrap(),
            Token::LeftAngle(..) => self
                .builder
                .build_int_compare(IntPredicate::SLT, left, right, "slt")
                .unwrap(),
            Token::GreaterEqual(..) => self
                .builder
                .build_int_compare(IntPredicate::SGE, left, right, "sge")
                .unwrap(),
            Token::RightAngle(..) => self
                .builder
                .build_int_compare(IntPredicate::SGT, left, right, "sgt")
                .unwrap(),
            Token::Percent(..) => self
                .builder
                .build_int_signed_rem(left, right, "srem")
                .unwrap(),
            _ => unimplemented!("Other binary operators"),
        }
    }

    /// Generates LLVM IR for variable expressions.
    fn generate_variable(&self, token: &Token) -> BasicValueEnum<'ctx> {
        let name = token_value!(token);
        if let Some(function) = self.module.get_function(&name) {
            function.as_global_value().as_basic_value_enum()
        } else if let Some(variable) = self.symbol_table.get_variable(&name) {
            match variable {
                Variable::Immutable(basic_value) => *basic_value,
                Variable::Mutable(pointer_value) => self
                    .builder
                    .build_load(pointer_value.get_type(), *pointer_value, &name)
                    .expect("Failed to load variable"),
            }
        } else {
            panic!("Variable `{}` not found", name);
        }
    }

    /// Generates LLVM IR for call expressions.
    fn generate_call_expr(&mut self, call_expr: &CallExpr) -> Option<BasicValueEnum<'ctx>> {
        let callee_expr = self
            .generate_expression(&call_expr.callee)
            .expect("Callee expression not found");

        // callee_expr.get_name();
        // Returns variable name ^
        let callee = self
            .module
            .get_function(callee_expr.get_name().to_str().unwrap())
            .expect("Callee function not found");

        if callee.count_params() != call_expr.arguments.len() as u32 {
            panic!("Incorrect # of arguments")
        }

        let args: Vec<BasicMetadataValueEnum> = call_expr
            .arguments
            .iter()
            .map(|arg| self.generate_expression(arg).unwrap().into())
            .collect();

        if callee.get_type().get_return_type().is_none() {
            None
        } else {
            Some(
                self.builder
                    .build_call(callee, &args, "call")
                    .expect("Failed to build call")
                    .try_as_basic_value()
                    .left()
                    .expect("Return type unsupported yet"),
            )
        }
    }

    /// Generates LLVM IR for if expressions.
    fn generate_if_expr(&mut self, if_expr: &IfExpr) -> Option<BasicValueEnum<'ctx>> {
        // After lowering, it is ensured that there is always and else branch,
        // and that elif statements, if any, are within the else branch.
        let function = self
            .builder
            .get_insert_block()
            .expect("No insertion block")
            .get_parent()
            .expect("No parent");

        let then_block = self.context.append_basic_block(function, "then");
        let else_block = self.context.append_basic_block(function, "else");
        let exit_block = self.context.append_basic_block(function, "exit");

        let cond_value = self
            .generate_expression(&if_expr.condition)
            .expect("Failed to generate conditional value");

        self.builder
            .build_conditional_branch(cond_value.into_int_value(), then_block, else_block)
            .unwrap();

        self.builder.position_at_end(then_block);
        let then_value = self.generate_block(&if_expr.then_branch, Vec::new())?;
        self.builder.build_unconditional_branch(exit_block).unwrap();

        self.builder.position_at_end(else_block);
        let else_value = if let Some(ref else_block) = if_expr.else_branch {
            self.generate_block(else_block, Vec::new())
        } else {
            None
        };

        self.builder.build_unconditional_branch(exit_block).unwrap();

        self.builder.position_at_end(exit_block);
        let phi_node = self
            .builder
            .build_phi(self.context.i32_type(), "if_phi")
            .unwrap();

        phi_node.add_incoming(&[(&then_value, then_block)]);
        if let Some(else_value) = else_value {
            phi_node.add_incoming(&[(&else_value, else_block)]);
        }

        Some(phi_node.as_basic_value())
    }
}
