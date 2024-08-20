use crate::ast::{Declaration, FunctionDecl};
use crate::codegen::cart_type::CartType;
use crate::codegen::symbol_table::Variable;
use crate::codegen::types::create_function_type;
use crate::codegen::CodeGen;
use crate::token_value;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::BasicValue;
use inkwell::AddressSpace;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for a declaration.
    pub(super) fn generate_declaration(&mut self, declaration: &Declaration) {
        match declaration {
            Declaration::FunctionDecl(ref func_decl) => self.generate_function(func_decl),
            Declaration::StatementDecl(ref stmt) => self.generate_statement(stmt),
            Declaration::StructDecl(ref struct_decl) => self.generate_struct(struct_decl),
            Declaration::ExtensionDecl(ref extension_decl) => {
                self.generate_extension(extension_decl)
            }
            _ => todo!(),
        };
    }

    /// Generates the LLVM IR for a function declaration.
    fn generate_function(&mut self, function_decl: &FunctionDecl) {
        let function_type = create_function_type(
            self.context,
            function_decl.return_type.to_any_type_enum(self.context),
            function_decl
                .params
                .iter()
                .map(|p| {
                    p.param_type
                        .to_basic_type_enum(self.context)
                        .expect("Void type in params")
                })
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
            variables_to_add.push((
                name,
                Variable::ImmutableParam(param.get_type().into(), param),
            ));
        }

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let basic_value_enum = self.generate_block(&function_decl.body, variables_to_add);
        let basic_value = basic_value_enum
            .as_ref()
            .map(|(_, val)| val as &dyn BasicValue);
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

    /// Generates the LLVM IR for a struct declaration.
    fn generate_struct(&mut self, struct_decl: &crate::ast::StructDecl) {
        let field_types = struct_decl
            .fields
            .iter()
            .map(|field| {
                field
                    .field_type
                    .to_basic_type_enum(self.context)
                    .expect("Invalid field type")
            })
            .collect::<Vec<BasicTypeEnum>>();

        let struct_type = self.context.struct_type(&field_types, false);

        let fields_to_indices_and_types = struct_decl
            .fields
            .iter()
            .enumerate()
            .map(|(i, field)| {
                (
                    token_value!(&field.name, Identifier),
                    (
                        i,
                        field
                            .field_type
                            .to_basic_type_enum(self.context)
                            .expect("Can't have void type in fields")
                            .into(),
                    ),
                )
            })
            .collect();

        self.symbol_table.add_variable(
            token_value!(&struct_decl.name),
            Variable::StructDecl(struct_type, fields_to_indices_and_types),
        );
    }

    /// Generates the LLVM IR for an extension declaration.
    /// Extensions are used to add methods to existing types.
    fn generate_extension(&mut self, extension_decl: &crate::ast::ExtensionDecl) {
        // TODO: Generics
        let struct_name = token_value!(&extension_decl.name);
        let struct_type = match self.symbol_table.get_variable(&struct_name).unwrap() {
            Variable::StructDecl(struct_type, _) => struct_type.as_basic_type_enum(),
            _ => panic!("Expected struct type"),
        };
        for method in extension_decl.functions.iter() {
            let mut function_params: Vec<BasicTypeEnum> = Vec::new();
            if method.is_self {
                function_params.push(
                    self.context
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                );
            }
            function_params.extend(
                method
                    .params
                    .iter()
                    .map(|param| param.param_type.to_basic_type_enum(self.context).unwrap()),
            );

            let function_type = create_function_type(
                self.context,
                method.return_type.to_any_type_enum(self.context),
                function_params,
            );

            let function = self.module.add_function(
                &format!("{}-{}", &struct_name, &token_value!(&method.name)),
                function_type,
                None, // Linkage::External
            );
            // self.symbol_table.add_function_to_struct(&struct_name, &token_value!(&method.name));

            let mut variables_to_add = Vec::new();
            if method.is_self {
                let self_param = function.get_first_param().unwrap();
                self_param.set_name("self");
                let cart_ty: CartType<'ctx> = struct_type.into();
                variables_to_add.push((
                    "self".to_string(),
                    // There is no option other than to clone: String has to be used multiple times
                    Variable::ImmutableParam(
                        cart_ty.with_name(struct_name.clone()).with_alloca(),
                        self_param,
                    ),
                ));

                // Skipping the first parameter as it is set above as self.
                for (i, param) in function.get_param_iter().skip(1).enumerate() {
                    let name = token_value!(&method.params[i].name);
                    param.set_name(&name);
                    variables_to_add.push((
                        name,
                        Variable::ImmutableParam(param.get_type().into(), param),
                    ));
                }
            } else {
                for (i, param) in function.get_param_iter().enumerate() {
                    let name = token_value!(&method.params[i].name);
                    param.set_name(&name);
                    variables_to_add.push((
                        name,
                        Variable::ImmutableParam(param.get_type().into(), param),
                    ));
                }
            }

            let basic_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(basic_block);

            let basic_value_enum = self.generate_block(&method.body, variables_to_add);
            let basic_value = basic_value_enum
                .as_ref()
                .map(|(_, val)| val as &dyn BasicValue);
            self.builder.build_return(basic_value).unwrap();
        }
    }
}
