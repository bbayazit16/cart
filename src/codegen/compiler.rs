use crate::ast::lowering::lower;
use crate::ast::Program;
use crate::cli::CommonOptions;
use crate::codegen::CodeGen;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use std::path::{Path, PathBuf};

/// Compiles the program into an executable.
/// Arguments:
/// - `ast` - The program AST.
/// - `context` - The inkwell context.
/// - `run` - If true, runs the executable after compilation.
/// - `options` - The compiler options.
pub(crate) fn compile(ast: &mut Program, run: bool, options: &CommonOptions) {
    lower(ast);

    let context = inkwell::context::Context::create();

    let mut codegen = CodeGen::new(&context, Some(&options.entrypoint));
    let module = codegen.generate(ast);

    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");

    if options.emit_ir {
        eprintln!();
        eprintln!("==========================================");
        eprintln!("================ Begin IR ================");
        eprintln!("==========================================");
        eprintln!();
        module.print_to_stderr();
        eprintln!();
        eprintln!("========================================");
        eprintln!("================ End IR ================");
        eprintln!("========================================");
        eprintln!();
    }

    let output_file_path = options
        .output
        .as_deref()
        .map(|p| PathBuf::from(p).with_extension("o"))
        .unwrap_or_else(|| {
            let input_file_without_extension = options.input.file_stem().unwrap();
            let mut output_path = PathBuf::from(input_file_without_extension);
            output_path.set_extension("o");
            output_path
        });

    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).expect("Unable to create target from triple");
    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            options.optimization.into(),
            RelocMode::Default,
            CodeModel::Default,
        )
        .expect("Unable to create target machine");

    target_machine
        .write_to_file(module, FileType::Object, Path::new(&output_file_path))
        .expect("Failed to write object file");

    let output_executable_path =
        options
            .output
            .as_deref()
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                let input_file_without_extension = options.input.file_stem().unwrap();
                PathBuf::from(input_file_without_extension)
            });

    link_object_file(&output_file_path, &output_executable_path);

    if run {
        run_executable(&output_executable_path);
    }
}

/// Links an object file into an executable.
fn link_object_file(object_file: &PathBuf, output_executable: &PathBuf) {
    let status = std::process::Command::new("cc")
        .arg(object_file)
        .arg("-o")
        .arg(output_executable)
        .arg("-L.")
        .arg("-lcartstd")
        .status()
        .expect("Failed to execute linker");

    if !status.success() {
        panic!("Linking failed");
    }

    std::fs::remove_file(object_file).expect("Failed to remove object file");
}

/// Runs the program executable.
fn run_executable(executable: &PathBuf) {
    let executable_path = if executable.is_absolute() {
        executable.clone()
    } else {
        std::env::current_dir().unwrap().join(executable)
    };

    let status = std::process::Command::new(&executable_path)
        .current_dir(std::env::current_dir().unwrap())
        .status()
        .expect("Failed to execute program");

    println!(
        "Program finished with exit code: {}",
        status.code().unwrap()
    );
}
