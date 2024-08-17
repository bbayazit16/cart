//! Reporter module defines the `Reporter` struct that is responsible for
//! reporting the compile-time errors, including syntax, type, and IO Errors.
//!
//! The reporter operates by taking in an error, as defined in the `errors` module.
use crate::context::FilePointer;
use crate::errors::{CompileError, Help, Position, SyntaxError, TypeError};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

const LINE_CONTEXT: usize = 2;
const ANSI_RED: &str = "\x1b[31m";
const ANSI_GREEN: &str = "\x1b[32m";
const ANSI_BOLD: &str = "\x1b[1m";
const ANSI_RESET: &str = "\x1b[0m";

/// Reporter formats and reports syntax, type, IO errors, and many other errors that may
/// occur during compilation in a nicely-formatted way.
///
/// To use the reporter struct, initialize a `Reporter` instance by passing in `file_path`.
/// Then, report errors via the `report` function.
///
/// The reporter uses `LINE_CONTEXT` variable to determine the number of additional lines
/// that will be read and displayed before reporting the error.
///
/// # Fields
/// - `file_path`: The path to file where the errors are occurring.
#[derive(Debug, Clone)]
pub(crate) struct Reporter {
    file_path: PathBuf,
}

impl Reporter {
    /// Initialize a reporter instance by passing in `file_path`,
    /// which must implement AsRef<Path>.
    pub(crate) fn new<P: AsRef<Path>>(file_path: P) -> Reporter {
        Reporter {
            file_path: file_path.as_ref().to_path_buf(),
        }
    }

    /// Report a `CompileError`.
    /// The function forwards the errors to `print_error`, with the `CompileError` converted
    /// into a string.
    pub(crate) fn report(&self, error: &CompileError) {
        match error {
            CompileError::Syntax(ref syntax_error) => self.report_syntax_error(syntax_error),
            CompileError::IO(ref io_error) => Self::report_io_error::<&str>(io_error, None),
            CompileError::TypeError(ref type_error) => self.report_type_error(type_error),
        }
    }

    /// Reports a Syntax Error to stderr.
    /// Forwards the message to `print_error`.
    fn report_syntax_error(&self, error: &SyntaxError) {
        self.print_error(
            error.to_string(),
            error.file_position(),
            error.help_message(),
        )
    }

    /// Reports a Type Error to stderr.
    /// Forwards the message to `print_error`.
    fn report_type_error(&self, error: &TypeError) {
        self.print_error(error.to_string(), error.file_position(), None)
    }

    /// Reports an IO Error to stderr.
    /// Can optionally report a recovered message.
    pub(crate) fn report_io_error<S: AsRef<str>>(
        error: &std::io::Error,
        additional_message: Option<S>,
    ) {
        eprintln!(
            "{}{}Error occurred opening file: {error}{}",
            ANSI_BOLD, ANSI_RED, ANSI_RESET
        );
        if let Some(message) = additional_message {
            eprintln!("Occurred when reporting: {}", message.as_ref());
        }
    }

    /// Prints any given `message` implementing `AsRef<str>` at position `file_pointer`
    /// into stderr, in addition to any optional help message `help_message`.
    pub(crate) fn print_error<S: AsRef<str>>(
        &self,
        message: S,
        file_pointer: FilePointer,
        help_message: Option<S>,
    ) {
        let file = match File::open(&self.file_path) {
            Ok(file) => file,
            Err(e) => {
                Self::report_io_error(
                    &e,
                    Some(format!(
                        "message={}{}",
                        message.as_ref(),
                        match help_message {
                            Some(help) => format!(" help={}", help.as_ref()),
                            None => String::from(""),
                        }
                    )),
                );
                return;
            }
        };

        let reader = BufReader::new(file);

        self.print_error_header(&message, &file_pointer);
        self.print_error_lines(reader, file_pointer);
        if let Some(help) = help_message {
            self.print_help_message(help);
        }
    }

    /// Prints the error header.
    fn print_error_header<S: AsRef<str>>(&self, message: &S, file_pointer: &FilePointer) {
        eprintln!(
            "{}{}error: {}{}",
            ANSI_BOLD,
            ANSI_RED,
            ANSI_RESET,
            message.as_ref()
        );

        eprintln!(
            "  --> {}:{}:{}",
            self.file_path.display(),
            file_pointer.line,
            file_pointer.file_position
        );
        eprintln!();
    }

    /// Prints the error lines to provide context and highlight the error.
    fn print_error_lines(&self, mut reader: BufReader<File>, file_pointer: FilePointer) {
        let start_line = if file_pointer.line > LINE_CONTEXT {
            file_pointer.line - LINE_CONTEXT
        } else {
            1
        };

        let end_line = file_pointer.line + LINE_CONTEXT;

        let mut current_line = 1;
        let mut line = String::new();

        while current_line <= end_line {
            match reader.read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    if current_line >= start_line {
                        self.print_line(current_line, &line, file_pointer);
                    }
                    current_line += 1;
                    line.clear();
                }
                Err(ref io_err) => {
                    Self::report_io_error::<&str>(io_err, None);
                    break;
                }
            }
        }
    }

    /// Prints a single line of error.
    fn print_line(&self, line_number: usize, line_content: &str, file_pointer: FilePointer) {
        let line_number_width = file_pointer.line.to_string().len();
        let prefix = if line_number == file_pointer.line {
            format!("{}{}>{}", ANSI_BOLD, ANSI_RED, ANSI_RESET)
        } else {
            " ".to_string()
        };

        eprintln!(
            "{}{:>width$} | {}",
            prefix,
            line_number,
            line_content.trim_end(),
            width = line_number_width
        );

        if line_number == file_pointer.line {
            let padding = " ".repeat(file_pointer.line_position - 1);
            let caret = format!("{}{}^{}", ANSI_BOLD, ANSI_RED, ANSI_RESET);
            eprintln!(
                " {:>width$} | {}{}",
                "",
                padding,
                caret,
                width = line_number_width
            );
        }
    }

    // Prints the help message.
    fn print_help_message<S: AsRef<str>>(&self, help_message: S) {
        eprintln!(
            "\n{}{}help:{} {}\n",
            ANSI_BOLD,
            ANSI_GREEN,
            ANSI_RESET,
            help_message.as_ref()
        );
    }
}
