use crate::ast::{Block, Declaration, Expr, Program, Stmt};

/// Lower the AST to prepare it for code generation.
pub(crate) fn lower(program: &mut Program) {
    for decl in &mut program.declarations {
        // TODO: Extensions, statements, etc. have blocks.
        if let Declaration::FunctionDecl(ref mut func_decl) = *decl {
            func_decl.body.lower();
        }
    }
}

// TODO: Add support for conditional assignments early function returns
impl Block {
    /// Lowers both return statements and if expressions in the block.
    fn lower(&mut self) {
        self.lower_if_expressions();
        self.lower_return();
    }

    /// Lowers the return statements in the block.
    /// If a return statement is detected, then:
    ///     1) Replace the return statement
    ///     2) Remove all leading statements in the block to eliminate dead code
    ///     3) Set block's return value to that expression
    ///     4) Break. There is only one return statement allowed in a block.
    /// This approach works because this function requires lower_if_expressions
    /// to be called first, which ensures that all if expressions have an else
    /// branch.
    fn lower_return(&mut self) {
        // First of all, identify the return statement in the block.
        let mut return_index = None;
        let mut return_expr = None;

        for (i, declaration) in self.declarations.iter().enumerate() {
            if let Declaration::StatementDecl(ref stmt) = declaration {
                if let Stmt::Return(ref expr) = **stmt {
                    return_index = Some(i);
                    return_expr = expr.clone();
                    break;
                }
            }
        }

        // If there is a return statement, then:
        if let Some(index) = return_index {
            // Remove return and everything after it from the block's declarations
            self.declarations.drain(index..);

            // Set the block's return expression
            self.return_expr = return_expr.map(Box::new);
        }
    }

    /// Lower each if expression in the block.
    /// This assigns each if expression an else branch. This way,
    /// the AST gets closer to LLVM IR representation, and early
    /// returns  are put into the else block and can be handled
    /// easier during codegen.
    fn lower_if_expressions(&mut self) {
        let mut i = 0;
        while i < self.declarations.len() {
            let (left, right) = self.declarations.split_at_mut(i + 1);
            let mut drain_range = 0;
            let mut should_drain = false;
            if let Declaration::StatementDecl(ref mut stmt) = left[i] {
                if let Stmt::Expression(ref mut expr) = **stmt {
                    if let Expr::If(ref mut if_expr) = **expr {
                        // Pass the rest of the declarations to the lower_if_expr function
                        if right.is_empty() {
                            return;
                        }

                        if_expr.then_branch.lower();

                        // TODO: Elif branches
                        if if_expr.else_branch.is_none() {
                            let mut else_block = Block {
                                declarations: right.to_vec(),
                                return_expr: self.return_expr.take(),
                            };
                            else_block.lower();

                            if_expr.else_branch = Some(Box::new(else_block));
                        } else if let Some(ref mut else_block) = if_expr.else_branch {
                            else_block.declarations.extend(right.to_vec());
                            else_block.lower();
                            else_block.return_expr = self.return_expr.take();
                        }

                        should_drain = true;
                        drain_range = i + 1;
                    }
                }
            }
            if should_drain {
                // Remove the declarations following if expression, they were
                // already moved to the else branch
                self.declarations.drain(drain_range..);
            }
            i += 1;
        }

        // Here, there's one subtle thing to note. Until now, we have only
        // iterated over the declarations in the block. Now, we must set the
        // block's return value to the if expression.

        // The popped value is the if expression that was just lowered.
        let if_expr = self.declarations.last();
        if let Some(Declaration::StatementDecl(ref stmt)) = if_expr {
            if let Stmt::Expression(expr) = &**stmt {
                if let Expr::If(_) = &**expr {
                    let last = self.declarations.pop().unwrap(); // removes the if expression
                    if let Declaration::StatementDecl(stmt) = last {
                        if let Stmt::Expression(if_expr) = *stmt {
                            self.return_expr = Some(if_expr);
                        }
                    }
                }
            }
        }
    }
}
