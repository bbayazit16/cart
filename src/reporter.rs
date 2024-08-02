use crate::error::{LexerError, SyntaxError};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

const LINE_CONTEXT: usize = 2;
const ANSI_RED: &str = "\x1b[31m";
const ANSI_GREEN: &str = "\x1b[32m";
const ANSI_BOLD: &str = "\x1b[1m";
const ANSI_RESET: &str = "\x1b[0m";

pub struct Reporter {
    file_path: String,
}

impl Reporter {
    pub fn new(file_path: String) -> Self {
        Reporter { file_path }
    }

    pub fn report(&self, error: &LexerError) {
        match error {
            LexerError::Syntax(syntax_error) => match syntax_error {
                SyntaxError::UnterminatedStringLiteral {
                    line,
                    file_position,
                    line_position,
                } => {
                    self.print_error(
                        &error.to_string(),
                        *line,
                        *file_position,
                        *line_position,
                        Some("Add a closing quote"),
                    );
                }
                SyntaxError::UnexpectedCharacter {
                    line,
                    file_position,
                    line_position,
                    ..
                } => {
                    self.print_error(
                        &error.to_string(),
                        *line,
                        *file_position,
                        *line_position,
                        None,
                    );
                }
                SyntaxError::InvalidNumberLiteral {
                    line,
                    file_position,
                    line_position,
                    ..
                } => {
                    self.print_error(
                        &error.to_string(),
                        *line,
                        *file_position,
                        *line_position,
                        None,
                    );
                }
            },
            LexerError::IO(io_error) => {
                eprintln!("IO error: {}", io_error);
            }
        }
    }

    fn print_error(
        &self,
        message: &str,
        line: usize,
        file_position: usize,
        line_position: usize,
        help_message: Option<&str>,
    ) {
        let file = File::open(&self.file_path).expect("Unable to open file to display the error");
        let reader = BufReader::new(file);

        let lines: Vec<String> = reader.lines().map_while(Result::ok).collect();

        let line_number_width = lines.len().to_string().len();

        eprintln!("{}{}error: {}{}", ANSI_BOLD, ANSI_RED, ANSI_RESET, message);

        eprintln!(
            "  --> {}:{}:{}",
            Path::new(&self.file_path).display(),
            line,
            file_position
        );
        eprintln!();

        let start_line = if line > LINE_CONTEXT {
            line - LINE_CONTEXT
        } else {
            1
        };

        let end_line = std::cmp::min(line + LINE_CONTEXT, lines.len());

        for (i, line_content) in lines[start_line - 1..end_line].iter().enumerate() {
            let current_line = start_line + i;
            let prefix = if current_line == line {
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

            if current_line == line {
                let padding = " ".repeat(line_position - 1);
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

        if let Some(help) = help_message {
            eprintln!();
            eprintln!("{}{}help:{} {}", ANSI_BOLD, ANSI_GREEN, ANSI_RESET, help);
        }

        eprintln!();
    }
}
