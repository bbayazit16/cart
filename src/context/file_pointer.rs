use std::fmt::Formatter;

/// The `FilePointer` struct represents a specific position in a text file,
/// including the line number, the position within the entire file,
/// and the position within the line.
///
/// `FilePointer` struct also implements display in the following format:
///
/// `line {line}, position {line_position}`
///
/// which is useful for error reporting.
///
/// The `FilePointer` struct keeps track of the following:
/// - `line`: The line number (1-indexed).
/// - `file_position`: The position (byte offset) within the entire file (0-indexed).
/// - `line_position`: The position within the line (1-indexed).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct FilePointer {
    pub line: usize,
    pub file_position: usize,
    pub line_position: usize,
}

impl Default for FilePointer {
    fn default() -> Self {
        FilePointer {
            line: 1,
            file_position: 0,
            line_position: 1,
        }
    }
}

impl std::fmt::Display for FilePointer {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "line {}, position {}", self.line, self.line_position)
    }
}
