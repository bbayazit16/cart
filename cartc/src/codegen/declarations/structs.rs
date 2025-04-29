use crate::codegen::CodeGen;
use crate::hir::Type;
use inkwell::types::BasicTypeEnum;
use std::collections::HashMap;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for a struct declaration.
    ///
    /// Steps to generate a struct are as follows:
    ///     1) Create a struct type.
    ///     2) Add the struct to the symbol table.
    pub(super) fn generate_struct(&mut self, name: &str, fields: &[(String, Type)]) {
        let field_llvm_types = fields
            .iter()
            .map(|(_, ty)| self.to_basic_type_enum(ty).unwrap())
            .collect::<Vec<BasicTypeEnum>>();

        // 1) Create a struct type.
        let struct_type = self.context.struct_type(&field_llvm_types, false);
        let fields_to_indices_and_types: HashMap<String, (usize, BasicTypeEnum)> = fields
            .iter()
            .enumerate()
            .map(|(i, (name, ty))| (name.clone(), (i, self.to_basic_type_enum(ty).unwrap())))
            .collect();

        // 2) Add the struct to the symbol table.
        self.struct_definition_table
            .add(name.to_string(), (struct_type, fields_to_indices_and_types));

        // TODO: Why was this commented, what was it here for before, etc.
        // let field_types = struct_decl
        //     .fields
        //     .iter()
        //     .map(|field| {
        //         field
        //             .field_type
        //             .to_basic_type_enum(self.context)
        //             .expect("Invalid field type")
        //     })
        //     .collect::<Vec<BasicTypeEnum>>();
        //
        // let struct_type = self.context.struct_type(&field_types, false);
        //
        // let fields_to_indices_and_types = struct_decl
        //     .fields
        //     .iter()
        //     .enumerate()
        //     .map(|(i, field)| {
        //         (
        //             token_value!(&field.name, Identifier),
        //             (
        //                 i,
        //                 field
        //                     .field_type
        //                     .to_basic_type_enum(self.context)
        //                     .expect("Can't have void type in fields")
        //                     .into(),
        //             ),
        //         )
        //     })
        //     .collect();
        //
        // self.symbol_table.add(
        //     token_value!(&struct_decl.name),
        //     Variable::StructDecl(struct_type, fields_to_indices_and_types),
        // );
    }
}
