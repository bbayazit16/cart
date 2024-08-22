use crate::hir::{Type, TypeChecker};
use crate::token::Token;
use crate::{ast, hir, token_value};
use std::collections::HashMap;

/// Given a string, determine the lowest amount of bits required to represent the integer.
/// If the number doesn't fit in any of the types, panic.
/// The bits must be one of the following: 32, 64, 128, 256.
///
/// Returned type is the HIR Number type.
fn determine_bit_size(_number_str: &str) -> Type {
    // TODO: Actually implement this function. The implementation was delayed due to unsigned ints.
    Type::Int
}

impl<'a> TypeChecker {
    /// Resolve the types of the AST nodes.
    pub(crate) fn resolve_types(&mut self, ast: &'a mut ast::Program) -> hir::Program {
        for declaration in &ast.declarations {
            if let ast::Declaration::FunctionDecl(ref function_decl) = declaration {
                let function_signature = self.resolve_function_signature(function_decl);
                // Register function signatures. This allows functions defined later in the source
                // code to be called by the functions defined earlier.
                // TODO: Do this for extensions as well.
                self.functions
                    .add(function_signature.name.to_string(), function_signature);
            }
        }

        let mut declarations = Vec::new();
        for declaration in ast.declarations.iter() {
            declarations.push(self.resolve_declaration(declaration));
        }

        hir::Program { declarations }
    }

    /// Resolve the types of the given declaration.
    fn resolve_declaration(&mut self, declaration: &'a ast::Declaration) -> hir::Declaration {
        match declaration {
            ast::Declaration::FunctionDecl(ref function_decl) => {
                hir::Declaration::Function(self.resolve_function_decl(function_decl, None))
            }
            ast::Declaration::StatementDecl(ref stmt) => {
                hir::Declaration::Statement(self.resolve_statement(stmt))
            }
            ast::Declaration::StructDecl(ref struct_decl) => self.resolve_struct_decl(struct_decl),
            ast::Declaration::ExtensionDecl(ref extension_decl) => {
                self.resolve_extension_decl(extension_decl)
            }
            e => unimplemented!("Resolve type for {:?}", e),
        }
    }

    /// Resolve the types of a function signature.
    fn resolve_function_signature(
        &mut self,
        function_decl: &'a ast::FunctionDecl,
    ) -> hir::FunctionSignature {
        let name = token_value!(&function_decl.name);

        for generic in function_decl.generic_params.iter() {
            self.add_ident_to_generics_table(generic);
        }

        let params = function_decl
            .params
            .iter()
            .map(|p| self.resolve_type(&p.param_type))
            .collect();
        let return_type = self.resolve_type(&function_decl.return_type);

        let generic_declarations = function_decl
            .generic_params
            .iter()
            .map(|p| self.resolve_type(p))
            .collect();
        let is_self = function_decl.is_self;

        hir::FunctionSignature {
            name,
            params,
            return_type,
            generic_declarations,
            is_self,
        }
    }

    /// Resolve the types of the given function declaration.
    fn resolve_function_decl(
        &mut self,
        function_decl: &'a ast::FunctionDecl,
        in_struct: Option<&str>,
    ) -> hir::Function {
        // Here, a scope is started because of function generics. If a generic is defined for the
        // function, it shouldn't be used for other functions.
        self.types.begin_scope();
        let signature = self.resolve_function_signature(function_decl);
        if let Some(struct_name) = in_struct {
            if signature.is_self {
                let self_type = Type::Struct(struct_name.to_string());
                // `self` variable is immutable.
                self.types.add("self".to_string(), (self_type, false));
            };

            match self.struct_methods.get_mut(struct_name) {
                Some(existing_methods) => {
                    if existing_methods.contains_key(&signature.name) {
                        panic!("Already implemented {} for {}", signature.name, struct_name);
                    }
                    existing_methods.insert(signature.name.clone(), signature.clone());
                }
                None => {
                    let methods = HashMap::from([(signature.name.clone(), signature.clone())]);
                    self.struct_methods.add(struct_name.to_string(), methods);
                }
            }
        }
        let body = self.resolve_block(&function_decl.body, Some(&function_decl.params));
        self.types.end_scope();

        // TODO: Better error handling
        assert_eq!(body.return_type, signature.return_type);

        hir::Function { signature, body }
    }

    /// Resolve the types of the given struct declaration.
    fn resolve_struct_decl(&mut self, struct_decl: &'a ast::StructDecl) -> hir::Declaration {
        let name = token_value!(&struct_decl.name);

        // Add struct to types BEFORE a generics type scope is started.
        self.types
            .add(name.clone(), (Type::Struct(name.clone()), false));

        // Here, a scope is started because of struct generics. If a generic is defined for the
        // struct, it shouldn't be used for other structs.
        self.types.begin_scope();
        self.generics_table.begin_scope();

        for generic in struct_decl.generic_params.iter() {
            self.add_ident_to_generics_table(generic);
            self.add_ident_to_struct_generics_table(&name, generic);
        }

        let fields = struct_decl
            .fields
            .iter()
            .map(|field| {
                (
                    token_value!(&field.name),
                    self.resolve_type(&field.field_type),
                )
            })
            .collect::<Vec<(String, Type)>>();

        let field_types = fields.iter().cloned().collect::<HashMap<String, Type>>();

        self.struct_fields.add(name.clone(), field_types);

        let generic_declarations = struct_decl
            .generic_params
            .iter()
            .map(|t| self.resolve_type(t))
            .collect::<Vec<Type>>();

        self.generics_table.end_scope();
        self.types.end_scope();

        hir::Declaration::Struct {
            name,
            fields,
            generic_declarations,
        }
    }

    /// Resolve the types of the given extension declaration.
    fn resolve_extension_decl(
        &mut self,
        extension_decl: &'a ast::ExtensionDecl,
    ) -> hir::Declaration {
        let struct_name = token_value!(&extension_decl.name);
        // Verify that the struct exists:
        self.struct_fields
            .get(&struct_name)
            .expect("Struct not found");

        // Here, a scope is started because of struct generics. If a generic is defined for the
        // struct, it shouldn't be used for other structs.
        self.generics_table.begin_scope();
        self.types.begin_scope();
        // DO NOT begin a struct methods scope here.
        // If a scope were to be started here, the methods would only be scoped to inside the
        // extension block. We want methods to be available everywhere the struct is available.

        let generics = if !extension_decl.generic_params.is_empty() {
            for generic in extension_decl.generic_params.iter() {
                self.add_ident_to_generics_table(generic);
            }

            let generics = extension_decl
                .generic_params
                .iter()
                .map(|t| self.resolve_type(t))
                .collect::<Vec<Type>>();

            let original_generics = self
                .struct_generics
                .get(&struct_name)
                .expect("Struct generics not found");
            // Verify that original generics and extension generics match.
            // They could have different names, but they should be the same type.
            // TODO: Check this approach?
            let generics_call = original_generics
                .iter()
                .map(|t| Type::DeclaredGenericType(t.clone()))
                .collect::<Vec<Type>>();
            let signatures_match = Self::verify_fn_call_types(&generics_call, &generics);
            // TODO: better err handling
            assert!(signatures_match, "Struct extension mismatch");

            generics
        } else {
            Vec::new()
        };

        let functions = extension_decl
            .functions
            .iter()
            .map(|f| self.resolve_function_decl(f, Some(&struct_name)))
            .collect();

        self.types.end_scope();
        self.generics_table.end_scope();

        hir::Declaration::Extension {
            struct_name,
            generics,
            functions,
        }
    }

    /// Resolve the types of the given block.
    fn resolve_block(
        &mut self,
        block: &'a ast::Block,
        params: Option<&'a Vec<ast::Parameter>>,
    ) -> hir::Block {
        self.types.begin_scope();
        self.functions.begin_scope();
        self.struct_fields.begin_scope();
        self.struct_methods.begin_scope();
        self.struct_generics.begin_scope();

        if let Some(params) = params {
            for param in params {
                let resolved_type = self.resolve_type(&param.param_type);
                self.types.add(
                    token_value!(&param.name),
                    // The is_mutable flag is always true for function parameters,
                    // and params are only passed when a function is being resolved.
                    (resolved_type, true),
                );
            }
        }

        let mut declarations = Vec::new();
        for declaration in &block.declarations {
            declarations.push(self.resolve_declaration(declaration));
        }

        let return_expr = block
            .return_expr
            .as_ref()
            .map(|return_expr| self.resolve_expr(return_expr));

        // map_or doesn't work, value is being moved!
        let return_type = if let Some(ref return_expr) = return_expr {
            return_expr.resulting_type()
        } else {
            Type::Unit
        };

        self.struct_generics.end_scope();
        self.struct_methods.end_scope();
        self.struct_fields.end_scope();
        self.functions.end_scope();
        self.types.end_scope();

        hir::Block {
            declarations,
            return_expr,
            return_type,
        }
    }

    /// Resolve the types of the given statement.
    fn resolve_statement(&mut self, stmt: &'a ast::Stmt) -> hir::Statement {
        match stmt {
            ast::Stmt::Expression(ref expr) => hir::Statement::Expression(self.resolve_expr(expr)),
            ast::Stmt::Let(ref let_stmt) => self.resolve_let_stmt(let_stmt),
            e => unimplemented!("Resolve type for {:?}", e),
        }
    }

    /// Resolve the types of the given let statement.
    fn resolve_let_stmt(&mut self, let_stmt: &'a ast::LetStmt) -> hir::Statement {
        let name = token_value!(&let_stmt.name);
        let is_mut = let_stmt.is_mut;

        let value = self.resolve_expr(&let_stmt.expr);
        let ty = value.resulting_type();

        if let Some(set_type) = &let_stmt.set_type {
            let set_type = self.resolve_type(set_type);

            assert_eq!(ty, set_type, "Type mismatch in let statement");
        }

        self.types.add(name.to_string(), (ty.clone(), is_mut));

        hir::Statement::Let {
            name,
            is_mut,
            ty,
            value,
        }
    }

    /// Resolve the types of the given expression.
    fn resolve_expr(&mut self, expr: &'a ast::Expr) -> hir::Expression {
        match expr {
            ast::Expr::Literal(ref literal) => self.resolve_literal(literal),
            ast::Expr::Variable(ref var, ..) => self.resolve_variable(var),
            ast::Expr::ArrayLiteral(ref elements) => self.resolve_array_literal(elements),
            ast::Expr::ArrayAccess(ref array_access) => self.resolve_array_access(array_access),
            ast::Expr::Binary(ref binary_expr) => self.resolve_binary_expression(binary_expr),
            ast::Expr::Unary(ref unary_expr) => self.resolve_unary_expression(unary_expr),
            ast::Expr::Call(ref call_expr) => self.resolve_call(call_expr),
            ast::Expr::MethodCall(ref method_call) => self.resolve_method_call(method_call),
            ast::Expr::StructLiteral(ref struct_literal) => {
                self.resolve_struct_literal(struct_literal)
            }
            ast::Expr::StructAccess(ref struct_access) => self.resolve_struct_access(struct_access),
            ast::Expr::Assignment(ref assignment) => self.resolve_assignment(assignment),
            ast::Expr::If(ref if_expr) => self.resolve_if_expr(if_expr),
            ast::Expr::Block(ref block) => {
                hir::Expression::Block(Box::new(self.resolve_block(block, None)))
            }
            ast::Expr::NotRecovered => unimplemented!("NotRecovered"),
            e => unimplemented!("Resolve type for {:?}", e),
        }
    }

    /// Resolve the types of the given literal.
    fn resolve_literal(&mut self, literal: &'a ast::Literal) -> hir::Expression {
        match literal {
            ast::Literal::Integer(ref tok) => {
                let value = token_value!(tok);
                let ty = determine_bit_size(&value);
                hir::Expression::Literal { value, ty }
            }
            ast::Literal::Float(ref tok) => {
                let value = token_value!(tok);
                // TODO: Float bit size
                let ty = Type::Float;
                hir::Expression::Literal { value, ty }
            }
            ast::Literal::Bool(ref tok) => {
                let value = token_value!(tok);
                let ty = Type::Bool;
                hir::Expression::Literal { value, ty }
            }
            ast::Literal::String(ref tok) => {
                let value = token_value!(tok);
                let ty = Type::String;
                hir::Expression::Literal { value, ty }
            }
            ast::Literal::NotRecovered => {
                // Actual None type implementation:
                // hir::Expression::Literal {
                //     value: "()".to_string(),
                //     ty: Type::Unit,
                // }
                unimplemented!("NotRecovered");
            }
        }
    }

    /// Resolve the types of the given variable.
    fn resolve_variable(&mut self, var: &'a Token) -> hir::Expression {
        let variable_name = token_value!(var);
        let ty = self.types.get(&variable_name);
        if let Some((ty, is_mut)) = ty {
            hir::Expression::Variable {
                name: variable_name,
                is_mut: *is_mut,
                ty: ty.clone(),
            }
        } else {
            // TODO: Better error handling
            unimplemented!("Variable not found: {}", variable_name);
        }
    }

    /// Resolve the types of the given array literal.
    fn resolve_array_literal(&mut self, elements: &'a [ast::Expr]) -> hir::Expression {
        let resolved_elements = elements
            .iter()
            .map(|e| self.resolve_expr(e))
            .collect::<Vec<hir::Expression>>();

        // TODO: Allow empty arrays with a type annotation.
        if resolved_elements.is_empty() {
            unimplemented!("Empty arrays are not yet supported.");
        }

        // TODO: Assure all elements have the same type.

        let element_type = resolved_elements[0].resulting_type();
        let array_type = Type::Array(Box::new(element_type.clone()));

        hir::Expression::ArrayLiteral {
            elements: resolved_elements,
            element_type,
            array_type,
        }
    }

    /// Resolve the types of the given array access expression.
    fn resolve_array_access(&mut self, array_access: &'a ast::ArrayAccessExpr) -> hir::Expression {
        let array = self.resolve_expr(&array_access.array);
        let element_type = match &array.resulting_type() {
            Type::Array(t) => *t.clone(),
            e => panic!("Array access on non-array type {:?}", e), // TODO: better error handling
        };

        let index = self.resolve_expr(&array_access.index);
        let index_type = index.resulting_type();

        hir::Expression::ArrayAccess {
            array: Box::new(array),
            element_type,
            index: Box::new(index),
            index_type,
        }
    }

    /// Resolve the types of the given binary expression.
    fn resolve_binary_expression(&mut self, binary_expr: &'a ast::BinaryExpr) -> hir::Expression {
        // TODO: Support casting
        let left = self.resolve_expr(&binary_expr.left);
        let right = self.resolve_expr(&binary_expr.right);
        let op = hir::BinaryOp::from(token_value!(&binary_expr.operator).as_str());

        let left_type = left.resulting_type();
        let right_type = right.resulting_type();
        let resulting_type = op.predict_output(&left_type, &right_type);

        hir::Expression::Binary {
            left: Box::new(left),
            left_type,
            op,
            right: Box::new(right),
            right_type,
            resulting_type,
        }
    }

    /// Resolve the types of the given unary expression.
    fn resolve_unary_expression(&mut self, unary_expr: &'a ast::UnaryExpr) -> hir::Expression {
        let expr = self.resolve_expr(&unary_expr.right);
        let ty = expr.resulting_type();
        let op = hir::UnaryOp::from(token_value!(&unary_expr.operator).as_str());

        hir::Expression::Unary {
            expr: Box::new(expr),
            ty,
            op,
        }
    }

    /// Resolve the types of the given call expression.
    fn resolve_call(&mut self, call_expr: &'a ast::CallExpr) -> hir::Expression {
        let callee = token_value!(&call_expr.callee);
        let arguments = call_expr
            .arguments
            .iter()
            .map(|arg| self.resolve_expr(arg))
            .collect::<Vec<hir::Expression>>();

        let arg_types = arguments
            .iter()
            .map(|arg| arg.resulting_type())
            .collect::<Vec<Type>>();

        let function_signature = self.functions.get(&callee).expect("Function not found");

        dbg!(&function_signature.params, &arg_types);
        let signatures_match = Self::verify_fn_call_types(&function_signature.params, &arg_types);

        // TODO: Better error handling
        assert!(signatures_match, "Function signature mismatch");

        hir::Expression::Call {
            callee,
            arguments,
            return_type: function_signature.return_type.clone(),
        }
    }

    /// Resolve the types of the given method call expression.
    fn resolve_method_call(&mut self, method_call: &'a ast::MethodCallExpr) -> hir::Expression {
        let object = self.resolve_expr(&method_call.object);
        let object_ty = object.resulting_type();
        let method = token_value!(&method_call.method_name);

        let arguments = method_call
            .arguments
            .iter()
            .map(|arg| self.resolve_expr(arg))
            .collect::<Vec<hir::Expression>>();

        let arg_types = arguments
            .iter()
            .map(|arg| arg.resulting_type())
            .collect::<Vec<Type>>();

        let object_name = Self::get_object_name(&object_ty);

        dbg!(&object_name);

        // TODO: Better error handling
        let struct_exists = self.types.get(&object_name).is_some();
        if !struct_exists {
            panic!("Struct not found: {}", object_name);
        }

        let method_signature = self
            .struct_methods
            .get(&object_name)
            .expect("Method not found") // If struct exists but there are no methods
            .get(&method)
            .expect("Method not found");

        let signatures_match = Self::verify_fn_call_types(&method_signature.params, &arg_types);
        // TODO: Better error handling
        assert!(signatures_match, "Function signature mismatch");

        hir::Expression::MethodCall {
            object: Box::new(object),
            object_ty,
            object_name,
            method,
            arguments,
            method_return_type: method_signature.return_type.clone(),
        }
    }

    /// Resolve the types of the given struct literal expression.
    fn resolve_struct_literal(
        &mut self,
        struct_literal: &'a ast::StructLiteral,
    ) -> hir::Expression {
        let struct_name = token_value!(&struct_literal.name);
        let struct_type = Type::Struct(struct_name.to_string());
        let fields = struct_literal
            .fields
            .iter()
            .map(|(token, expr)| (token_value!(token, Identifier), self.resolve_expr(expr)))
            .collect::<Vec<(String, hir::Expression)>>();

        // TODO: Verify struct args

        hir::Expression::StructLiteral {
            struct_name,
            struct_type,
            fields,
        }
    }

    /// Resolve the types of the given struct access expression.
    fn resolve_struct_access(
        &mut self,
        struct_access: &'a ast::StructAccessExpr,
    ) -> hir::Expression {
        let object = self.resolve_expr(&struct_access.object);
        let object_ty = object.resulting_type();

        let object_name = Self::get_object_name(&object_ty);

        // TODO: Multiple fields
        let field = token_value!(&struct_access.fields[0]);
        // TODO: Graceful error handling

        let returned_field_type = self
            .struct_fields
            .get(&object_name)
            .expect("Struct not found")
            .get(&field)
            .expect("Field not found")
            .clone();

        hir::Expression::StructAccess {
            object: Box::new(object),
            object_ty,
            object_name,
            field,
            returned_field_type,
        }
    }

    /// Resolve the types of the given assignment expression.
    fn resolve_assignment(&mut self, assignment: &ast::AssignmentExpr) -> hir::Expression {
        let l_value = self.resolve_expr(&assignment.l_value);
        let l_value_type = l_value.resulting_type();
        let r_value = self.resolve_expr(&assignment.r_value);
        let r_value_type = r_value.resulting_type();

        // Check if the variable is declared mutable
        // TODO: Better error handling
        assert_eq!(
            l_value_type, r_value_type,
            "Assignment type mismatch: {:?}, {:?}",
            l_value_type, r_value_type
        );
        if let hir::Expression::Variable { name, .. } = &l_value {
            let (_, is_mut) = self.types.get(name).expect("Variable not found");
            assert!(*is_mut, "Assignment to immutable variable");
        }

        hir::Expression::Assignment {
            l_value: Box::new(l_value),
            l_value_type,
            r_value: Box::new(r_value),
            r_value_type,
        }
    }

    /// Resolve the types of the given if expression.
    fn resolve_if_expr(&mut self, if_expr: &ast::IfExpr) -> hir::Expression {
        let condition = self.resolve_expr(&if_expr.condition);

        let then_branch = self.resolve_block(&if_expr.then_branch, None);
        let then_branch_type = &then_branch.return_type.clone();

        let elif_branches = if_expr
            .elif_branches
            .iter()
            .map(|(condition_expr, block)| {
                let condition = self.resolve_expr(condition_expr);
                let block = self.resolve_block(block, None);
                (condition, block)
            })
            .collect::<Vec<(hir::Expression, hir::Block)>>();

        let else_branch = if_expr
            .else_branch
            .as_ref()
            .map(|else_branch| Box::new(self.resolve_block(else_branch, None)));

        // TODO: Better error handling
        // Check that all elif branch types are the same
        let elif_branch_type = elif_branches
            .iter()
            .map(|(_, block)| &block.return_type)
            .fold(then_branch_type, |acc, ty| {
                assert_eq!(acc, ty, "If branch types do not match");
                acc
            });

        let return_types_match = if let Some(ref else_branch) = else_branch {
            let else_branch_type = &else_branch.return_type;
            then_branch_type == elif_branch_type && elif_branch_type == else_branch_type
        } else {
            then_branch_type == elif_branch_type
        };

        assert!(return_types_match, "If branch types do not match");

        hir::Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            elif_branches,
            else_branch,
            ty: then_branch_type.clone(), // All branches have the same type
        }
    }

    /// Recursively resolve the types of a generic type.
    fn resolve_type(&mut self, ty: &'a ast::Type) -> Type {
        match ty {
            ast::Type::Simple(ref tok) => self.resolve_non_generic_type(tok),
            ast::Type::Generic(ref name, ref inner_generics) => {
                let outer_generic = self.resolve_non_generic_type(name);
                let inner_generics = inner_generics
                    .iter()
                    .map(|g| self.resolve_type(g))
                    .collect::<Vec<Type>>();
                Type::Generic(Box::new(outer_generic), inner_generics)
            }
            ast::Type::Empty => Type::Unit,
            ast::Type::NotRecovered => unimplemented!("NotRecovered"),
        }
    }

    /// Resolve the types of a non-generic type.
    fn resolve_non_generic_type(&mut self, tok: &Token) -> Type {
        match token_value!(tok).as_str() {
            "int" => Type::Int,
            "in64" => Type::Int64,
            "int128" => Type::Int128,
            "int256" => Type::Int256,
            "float" => Type::Float,
            "bool" => Type::Bool,
            "string" => Type::String,
            "()" => Type::Unit,
            other => {
                if self.generics_table.contains(&other.to_string()) {
                    Type::DeclaredGenericType(other.to_string())
                } else if self.types.get(other).is_some() {
                    Type::Struct(other.to_string())
                } else {
                    panic!("Type not found: {}", other);
                }
            }
        }
    }

    /// Get the name of the object type.
    fn get_object_name(object_ty: &Type) -> String {
        match object_ty {
            Type::Struct(ref name) => name.to_string(),
            Type::Generic(..) | Type::DeclaredGenericType(..) => {
                unimplemented!("Generics are not yet supported")
            }
            _ => unimplemented!("Method calls are only supported on structs"),
        }
    }

    /// Add an identifier to generics table.
    fn add_ident_to_generics_table(&mut self, generic: &ast::Type) {
        match generic {
            ast::Type::Simple(ref name) => {
                self.generics_table.add(token_value!(name));
            }
            // TODO: better err handling here.
            _ => panic!("Generics can only be simple identifiers"),
        }
    }

    /// Add an identifier to struct generics table.
    fn add_ident_to_struct_generics_table(&mut self, struct_name: &str, generic: &ast::Type) {
        match generic {
            ast::Type::Simple(ref name) => {
                self.struct_generics.push(struct_name, token_value!(name));
            }
            // TODO: better err handling here.
            _ => panic!("Generics can only be simple identifiers (unknown generic)"),
        }
    }

    /// Verify that, in a function call, all types correspond, and that
    /// generics can be correctly substituted with the given types.
    /// For example, a function call foo(StructType<T>) can be verified against
    /// foo(StructType<Int>), including any nested generics.
    ///
    /// # Arguments
    /// - `arg_types`: The types of the arguments in the function signature.
    /// - `params`: The types of the parameters in the function call.
    fn verify_fn_call_types(arg_types: &[Type], params: &[Type]) -> bool {
        let mut generic_map: HashMap<&String, &Type> = HashMap::new();
        let signatures_match = params.len() == arg_types.len()
            && arg_types.iter().zip(params.iter()).all(|(a, b)| {
                match a {
                    Type::DeclaredGenericType(ref generic_typename) => {
                        if let Some(t) = generic_map.get(generic_typename) {
                            // The generic was previously registered in the generic map.
                            // Check if the registered filled type matches the current type.
                            *t == b
                        } else {
                            // Register the generic type in the generic map.
                            generic_map.insert(generic_typename, b);
                            true
                        }
                    }
                    // This match branch performs checks for types such as
                    // StructType<T, OtherStruct<T>> (or more complex ones). It ensures
                    // that the generic type T is filled with the correct type.
                    // This is a recursive type, so we need to compare the types recursively.
                    // In addition to the generic type checking, this match arm must also
                    // verify that the non-generic types match. For example, for the type
                    // StructType<T>, the StructType part must match.
                    Type::Generic(ref generic_name, ref generic_parts) => {
                        // Here, `generic_types` could be another generic type. That's why
                        // this comparison is recursive.
                        Self::recurse_into_generic_type(generic_name, generic_parts, b)
                    }
                    Type::Array(ref array_type) => {
                        // Array types can be nested: For example,
                        // Struct<T>[] is a valid type. Recursive type checking is required
                        // to ensure that the types match.
                        // Consider the example:
                        // - Struct<T, Struct<T>>[] against Struct<Int, Struct<Int>>[]
                        // 1) `array_type` is `Array(Struct(T, Struct(T)))`
                        // 2) `b` is `Array(Struct(Int, Struct(Int)))`
                        // (note for the algorithm: if `b` is not an array type, return false)
                        // Then, we want to verify that the inner types, which are:
                        // Struct(T, Struct(T)) match Struct(Int, Struct(Int))
                        match b {
                            Type::Array(ref array_type_b) => Self::verify_fn_call_types(
                                std::slice::from_ref(array_type),
                                std::slice::from_ref(array_type_b),
                            ),
                            _ => false,
                        }
                    }
                    _ => a == b, // Regular type comparison.
                }
            });
        signatures_match
    }

    /// Verify that the generic types match.
    ///
    /// # Arguments
    /// Consider the following example for the function arguments:
    ///
    /// StructType<T, OtherStruct<T>> against StructType<Int, OtherStruct<Int>>
    /// - `generic_name`: Struct("StructType"),
    /// - `generic_parts`: \[T, Generic(Struct("OtherStruct"), \[T])]
    /// - `b`: Generic(Struct("StructType"), \[Int, Generic(Struct("OtherStruct"), \[Int])])
    ///
    /// # Algorithm
    /// Steps:
    /// 1) Decouple `b` into (`generic_name_b`, `generic_parts_b`). If `b` is not a generic
    ///     type, return false. `b` should otherwise always be a generic type. Consider the
    ///     simplest scenario where:
    ///     - StructType<T> against StructType<Int>
    ///         Then:
    ///     - `generic_name`: Struct("StructType"),
    ///     - `generic_parts`: \[T]
    ///     - `b`: Generic(Struct("StructType"), \[Int])
    ///     - `generic_name_b`: Struct("StructType"),
    ///     - `generic_parts_b`: \[Int]
    ///
    /// Notice `b` is still a generic type. If not, then what is being validated against
    /// could never be a Generic type, return false.
    /// 2) Verify `generic_name` against the `generic_name_b`. If false, return false.
    ///      - Struct("StructType") == Struct("StructType")
    /// 3) Notice that `generic_parts` and `generic_parts_b` are both arrays. Now,
    ///     pass them back into `verify_generic_placeholders` to verify that the types match.
    ///    - Self::verify_generic_placeholders(generic_parts, generic_parts_b)
    /// 4) Return the result of the call.
    fn recurse_into_generic_type(generic_name: &Type, generic_parts: &[Type], b: &Type) -> bool {
        match b {
            Type::Generic(ref generic_name_b, ref generic_parts_b) => {
                if generic_name != generic_name_b.as_ref() {
                    return false;
                }

                Self::verify_fn_call_types(generic_parts, generic_parts_b)
            }
            _ => false,
        }
    }
}