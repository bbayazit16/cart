mod ast_codegen;
mod types;
mod symbol_table;
mod type_check;

pub mod compile {
    use crate::ast::{lower, Program};
    use crate::codegen::ast_codegen;
    use inkwell::targets::{InitializationConfig, Target};
    use inkwell::OptimizationLevel;

    /// Compile the AST to LLVM IR and return a closure that runs the compiled code.
    /// Assumes that a function with signature "main() -> i32" exists in the AST,
    /// and treats the function as the entry point.
    pub fn compile<'ctx>(ast: &mut Program, context: &'ctx inkwell::context::Context) -> Box<dyn Fn() -> i32 + 'ctx>{
        lower(ast);
        let mut codegen = ast_codegen::CodeGen::new(context, Some("main"));
        let module = codegen.generate(ast);
        
        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");
        
        // Temporary - print the generated IR to stderr
        module.print_to_stderr();
        
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();
        
        let main_func = unsafe {
            execution_engine
                .get_function::<unsafe extern "C" fn() -> i32>("main")
                .unwrap()
        };
        
        Box::new(move || unsafe {
            main_func.call()
        })
    }
}
