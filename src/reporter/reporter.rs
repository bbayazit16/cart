//! Reporter module defines the `Reporter` struct that is responsible for
//! reporting the compile-time errors, including syntax, type, and IO Errors.
//!
//! The reporter operates by taking in an error, as defined in the `errors` module.

use crate::context::FilePointer;
use crate::errors::{CompileError, Help, SyntaxError};
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
pub struct Reporter {
    file_path: PathBuf,
}

impl Reporter {
    /// Initialize a reporter instance by passing in `file_path`,
    /// which must implement AsRef<Path>.
    pub fn new<P: AsRef<Path>>(file_path: P) -> Reporter {
        Reporter { file_path: file_path.as_ref().to_path_buf() }
    }

    /// Report a `CompileError`.
    /// The function forwards the errors to `print_error`, with the `CompileError` converted
    /// into a string.
    pub fn report(&self, error: &CompileError) {
        match error {
            CompileError::Syntax(ref syntax_error) => self.report_syntax_error(syntax_error),
            CompileError::IO(ref io_error) => Self::report_io_error::<&str>(io_error, None),
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

    /// Reports an IO Error to stderr.
    /// Can optionally report a recovered message.
    pub fn report_io_error<S: AsRef<str>>(error: &std::io::Error, additional_message: Option<S>) {
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
    pub fn print_error<S: AsRef<str>>(
        &self,
        message: S,
        file_pointer: FilePointer,
        help_message: Option<S>,
    ) {
        let reader = match File::open(&self.file_path) {
            Ok(file) => BufReader::new(file),
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

        let lines: Vec<String> = reader.lines().map_while(Result::ok).collect();

        let line_number_width = lines.len().to_string().len();

        self.print_error_header(&message, &file_pointer);
        self.print_error_lines(&lines, file_pointer, line_number_width);
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

    /// Prints the main error.
    fn print_error_lines(
        &self,
        lines: &[String],
        file_pointer: FilePointer,
        line_number_width: usize,
    ) {
        let start_line = if file_pointer.line > LINE_CONTEXT {
            file_pointer.line - LINE_CONTEXT
        } else {
            1
        };

        let end_line = std::cmp::min(file_pointer.line + LINE_CONTEXT, lines.len());

        for (i, line_content) in lines[start_line - 1..end_line].iter().enumerate() {
            let current_line = start_line + i;
            let prefix = if current_line == file_pointer.line {
                format!("{}{}>{}", ANSI_BOLD, ANSI_RED, ANSI_RESET)
            } else {
                " ".to_string()
            };

            eprintln!(
                "{}{:>width$} | {}",
                prefix,
                current_line,
                line_content,
                width = line_number_width
            );

            if current_line == file_pointer.line {
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