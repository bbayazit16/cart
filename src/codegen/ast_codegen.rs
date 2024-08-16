//! ast_codegen module is responsible for generating LLVM IR from the AST.
//!
//! The CodeGen struct is the main struct that is used to generate the LLVM IR.
//!
use crate::ast::{
    Block, CallExpr, Declaration, Expr, FunctionDecl, IfExpr, LetStmt, Literal, Program, Stmt,
};

use crate::codegen::symbol_table::{SymbolTable, Variable};
use crate::codegen::types::create_function_type;
use crate::token::Token;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, IntValue};
use inkwell::IntPredicate;

macro_rules! token_value {
    ($ident:expr) => {
        match $ident {
            crate::token::Token::Identifier(_, s) => s.to_string(),
            crate::token::Token::Number(_, s, _) => s.to_string(),
            _ => panic!("Must be an identifier"),
        }
    };
}

/// Codegen struct is responsible for generating LLVM IR from the AST.
/// The struct contains the context, module, builder, and the symbol table.
/// The symbol table is used to keep track of the variables in the current scope.
/// The CodeGen struct is created with a context, and the generate method is called
/// with the program AST to generate the LLVM IR.
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbol_table: SymbolTable<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    /// Create a new CodeGen struct given context,
    /// optionally specifying the module name. If the module name
    /// isn't specified, the default name is "main".
    pub fn new<S: AsRef<str>>(context: &'ctx Context, module_name: Option<S>) -> Self {
        let module_name_str = module_name
            .as_ref()
            .map_or("main", |s| s.as_ref())
            .to_string();
        let module = context.create_module(&module_name_str);
        let builder = context.create_builder();
        CodeGen {
            context,
            module,
            builder,
            symbol_table: SymbolTable::default(),
        }
    }

    /// Generate the LLVM IR from the program AST.
    /// Returns a reference to the module.
    pub fn generate(&mut self, program: &Program) -> &Module<'ctx> {
        for declaration in program.declarations.iter() {
            self.generate_declaration(declaration);
        }
        &self.module
    }

    /// Generates the LLVM IR for a declaration.
    fn generate_declaration(&mut self, declaration: &Declaration) {
        match declaration {
            Declaration::FunctionDecl(ref func_decl) => self.generate_function(func_decl),
            Declaration::StatementDecl(ref stmt) => self.generate_statement(stmt),
            _ => todo!(),
        };
    }

    /// Generates the LLVM IR for a function declaration.
    fn generate_function(&mut self, function_decl: &FunctionDecl) {
        let function_type = create_function_type(
            self.context,
            function_decl.return_type.to_llvm_type(self.context),
            function_decl
                .params
                .iter()
                .map(|p| p.param_type.to_llvm_type(self.context))
                .collect(),
        );

        let function = self.module.add_function(
            &token_value!(&function_decl.name),
            function_type,
            None, // Linkage::External
        );

        let mut variables_to_add = Vec::new();
        for (i, param) in function.get_param_iter().enumerate() {
            let name = token_value!(&function_decl.params[i].name);
            param.set_name(&name);
            variables_to_add.push((name, Variable::Immutable(param)));
        }

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let basic_value_enum = self.generate_block(&function_decl.body, variables_to_add);
        // dbg!(&basic_value_enum);
        let basic_value = basic_value_enum.as_ref().map(|val| val as &dyn BasicValue);
        dbg!(&basic_block.get_terminator());
        self.builder.build_return(basic_value).unwrap();

        // // If there's a return value that's already generated, return that value.
        // if basic_block.get_terminator().is_none() {
        //     // No need to handle no return values here: since basic_value is still an Option
        //     // at this point, and build_return expects an Option<BasicValue>, we can just pass
        //     // basic_value directly.
        //     // Even if there's no return statement inside the block, and there's no last
        //     // value, this branch will run self.builder.build_return(None).
        //     self.builder.build_return(basic_value).unwrap();
        // }
    }

    /// Generates the LLVM IR for a statement.
    fn generate_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.generate_expression(expr);
            }
            Stmt::Return(_) => unreachable!("Return statement"),
            // Stmt::Return(ref expr) => {
            //     self.generate_return(expr);
            // }
            Stmt::Let(ref let_stmt) => {
                self.generate_let_stmt(let_stmt);
            }
            _ => todo!(),
        }
    }

    /// Generates the LLVM IR for an expression.
    fn generate_expression(&mut self, expr: &Expr) -> Option<BasicValueEnum<'ctx>> {
        // dbg!(&expr);
        match expr {
            Expr::Block(ref block) => self.generate_block(block, Vec::new()),
            Expr::Literal(ref literal) => self.generate_literal(literal),
            Expr::Binary(ref binary_expr) => Some(self.generate_binary(binary_expr)),
            Expr::Variable(ref token, ..) => Some(self.generate_variable(token)),
            Expr::Call(ref call_expr) => self.generate_call_expr(call_expr),
            Expr::IfExpr(ref if_expr) => self.generate_if_expr(if_expr),
            _ => todo!(),
        }
    }

    /// Generates the LLVM IR for a block.
    /// Optionally, pass a tuple of values that will be added as variables
    /// to the symbol table after starting the scope.
    fn generate_block(
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

    /// Generates LLVM IR for return expressions.
    // fn generate_return(&mut self, expr_opt: &Option<Expr>) {
    //     let return_value = expr_opt.as_ref().map(|expr| {
    //         let val = self.generate_expression(expr).unwrap();
    //         Box::new(val) as Box<dyn BasicValue>
    //     });
    //
    //     self.builder.build_return(return_value.as_deref()).unwrap();
    // }

    /// Generates LLVM IR for let statements.
    fn generate_let_stmt(&mut self, let_stmt: &LetStmt) {
        let value = self.generate_expression(&let_stmt.expr).unwrap();
        let name = token_value!(&let_stmt.name);
        if let_stmt.is_mut {
            let alloca = self
                .builder
                .build_alloca(value.get_type(), &name)
                .expect("Failed to allocate variable");
            // entry block alloca?
            self.builder.build_store(alloca, value).unwrap();
            self.symbol_table
                .add_variable(name, Variable::Mutable(alloca));
        } else {
            self.symbol_table
                .add_variable(name, Variable::Immutable(value));
        }
    }
}
