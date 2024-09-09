use crate::codegen::types::create_function_type;
use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{Declaration, Function};
use inkwell::values::BasicValue;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for a declaration.
    pub(super) fn generate_declaration(&mut self, declaration: &Declaration) {
        match declaration {
            Declaration::Function(ref function) => self.generate_function(function),
            Declaration::Statement(ref stmt) => self.generate_statement(stmt),
            // Declaration::StructDecl(ref struct_decl) => self.generate_struct(struct_decl),
            // Declaration::ExtensionDecl(ref extension_decl) => {
            //     self.generate_extension(extension_decl)
            // }
            _ => todo!(),
        };
    }

    /// Generates the LLVM IR for a function declaration.
    fn generate_function(&mut self, function_hir: &Function) {
        let function_type = create_function_type(
            self.context,
            &function_hir.signature.return_type,
            &function_hir
                .signature
                .params
                .iter()
                .map(|(_, ty)| ty)
                .collect::<Vec<_>>(),
        );

        let function = self.module.add_function(
            &function_hir.signature.name,
            function_type,
            None, // Linkage::External
        );

        let mut variables_to_add = Vec::new();
        for (i, param) in function.get_param_iter().enumerate() {
            let hir_param = &function_hir.signature.params[i];
            let name = &hir_param.0;
            param.set_name(name);
            variables_to_add.push((
                name,
                Value::new(param.get_type(), param.as_basic_value_enum()),
            ));
        }

        let bb_name = format!("{}-entry", &function_hir.signature.name);
        let basic_block = self.context.append_basic_block(function, &bb_name);
        self.builder.position_at_end(basic_block);

        let basic_value_enum = self.generate_block(&function_hir.body, variables_to_add);
        let basic_value = basic_value_enum
            .as_ref()
            .map(|value| &value.basic_value as &dyn BasicValue);
        
        self.builder.build_return(basic_value).unwrap();
    }

    // /// Generates the LLVM IR for a struct declaration.
    // fn generate_struct(&mut self, struct_decl: &crate::ast::StructDecl) {
    //     let field_types = struct_decl
    //         .fields
    //         .iter()
    //         .map(|field| {
    //             field
    //                 .field_type
    //                 .to_basic_type_enum(self.context)
    //                 .expect("Invalid field type")
    //         })
    //         .collect::<Vec<BasicTypeEnum>>();
    //
    //     let struct_type = self.context.struct_type(&field_types, false);
    //
    //     let fields_to_indices_and_types = struct_decl
    //         .fields
    //         .iter()
    //         .enumerate()
    //         .map(|(i, field)| {
    //             (
    //                 token_value!(&field.name, Identifier),
    //                 (
    //                     i,
    //                     field
    //                         .field_type
    //                         .to_basic_type_enum(self.context)
    //                         .expect("Can't have void type in fields")
    //                         .into(),
    //                 ),
    //             )
    //         })
    //         .collect();
    //
    //     self.symbol_table.add(
    //         token_value!(&struct_decl.name),
    //         Variable::StructDecl(struct_type, fields_to_indices_and_types),
    //     );
    // }
    //
    // /// Generates the LLVM IR for an extension declaration.
    // /// Extensions are used to add methods to existing types.
    // fn generate_extension(&mut self, extension_decl: &crate::ast::ExtensionDecl) {
    //     // TODO: Generics
    //     let struct_name = token_value!(&extension_decl.name);
    //     let struct_type = match self.symbol_table.get(&struct_name).unwrap() {
    //         Variable::StructDecl(struct_type, _) => struct_type.as_basic_type_enum(),
    //         _ => panic!("Expected struct type"),
    //     };
    //     for method in extension_decl.functions.iter() {
    //         let mut function_params: Vec<BasicTypeEnum> = Vec::new();
    //         if method.is_self {
    //             function_params.push(
    //                 self.context
    //                     .ptr_type(AddressSpace::default())
    //                     .as_basic_type_enum(),
    //             );
    //         }
    //         function_params.extend(
    //             method
    //                 .params
    //                 .iter()
    //                 .map(|param| param.param_type.to_basic_type_enum(self.context).unwrap()),
    //         );
    //
    //         let function_type = create_function_type(
    //             self.context,
    //             method.return_type.to_any_type_enum(self.context),
    //             function_params,
    //         );
    //
    //         let function = self.module.add_function(
    //             &format!("{}-{}", &struct_name, &token_value!(&method.name)),
    //             function_type,
    //             None, // Linkage::External
    //         );
    //         // self.symbol_table.add_function_to_struct(&struct_name, &token_value!(&method.name));
    //
    //         let mut variables_to_add = Vec::new();
    //         if method.is_self {
    //             let self_param = function.get_first_param().unwrap();
    //             self_param.set_name("self");
    //             let cart_ty: CartType<'ctx> = struct_type.into();
    //             variables_to_add.push((
    //                 "self".to_string(),
    //                 // There is no option other than to clone: String has to be used multiple times
    //                 Variable::ImmutableParam(
    //                     cart_ty.with_name(struct_name.clone()).with_alloca(),
    //                     self_param,
    //                 ),
    //             ));
    //
    //             // Skipping the first parameter as it is set above as self.
    //             for (i, param) in function.get_param_iter().skip(1).enumerate() {
    //                 let name = token_value!(&method.params[i].name);
    //                 param.set_name(&name);
    //                 variables_to_add.push((
    //                     name,
    //                     Variable::ImmutableParam(param.get_type().into(), param),
    //                 ));
    //             }
    //         } else {
    //             for (i, param) in function.get_param_iter().enumerate() {
    //                 let name = token_value!(&method.params[i].name);
    //                 param.set_name(&name);
    //                 variables_to_add.push((
    //                     name,
    //                     Variable::ImmutableParam(param.get_type().into(), param),
    //                 ));
    //             }
    //         }
    //
    //         let basic_block = self.context.append_basic_block(function, "entry");
    //         self.builder.position_at_end(basic_block);
    //
    //         let basic_value_enum = self.generate_block(&method.body, variables_to_add);
    //         let basic_value = basic_value_enum
    //             .as_ref()
    //             .map(|(_, val)| val as &dyn BasicValue);
    //         self.builder.build_return(basic_value).unwrap();
    //     }
    // }
}
