/// The `Position` struct represents a specific position in a text file,
/// including the line number, the position within the entire file,
/// and the position within the line.
///
/// The `FilePointer` struct keeps track of the following:
/// - `line`: The starting line number (1-indexed).
/// - `column`: The starting position within the line (1-indexed).
/// - `file_position`: The starting position (byte offset) within the entire file (0-indexed).
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    pub(crate) fn new(line: usize, column: usize, offset: usize) -> Self {
        Position {
            line,
            column,
            offset,
        }
    }

    /// Point to the character right after the current position.
    pub(crate) fn right_after(&self) -> Self {
        Position {
            line: self.line,
            column: self.column + 1,
            offset: self.offset + 1,
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Position {
            line: 1,
            column: 1,
            offset: 0,
        }
    }
}

/// A `Span` represents a range within a source file, used to denote the location of
/// a token, expression, or any other syntactic element in the code. A `Span` consists of a
/// starting `Position` and an ending `Position`, which describe the exact location of the
/// span in terms of lines, columns, and byte offsets.
///
/// The range is given as [start, end), meaning that the `end` position is excluded in the span.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub(crate) fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }

    /// Create a span spanning a single position.
    pub(crate) fn single(pos: Position) -> Self {
        Span {
            start: pos,
            end: Position {
                line: pos.line,
                column: pos.column + 1,
                offset: pos.offset + 1,
            },
        }
    }

    /// Create a span from:
    /// 1) Starting line, column, and offset.
    /// 2) Ending line, column, and offset.
    ///     Where both are inclusive.
    pub(crate) fn from_components(
        start_line: usize,
        start_column: usize,
        start_offset: usize,
        end_line: usize,
        end_column: usize,
        end_offset: usize,
    ) -> Self {
        Span {
            start: Position::new(start_line, start_column, start_offset),
            end: Position::new(end_line, end_column, end_offset),
        }
    }

    /// Return whether the span contains a given position.
    pub(crate) fn contains(&self, pos: &Position) -> bool {
        self.start <= *pos && *pos <= self.end
    }

    /// Check if two spans overlap (inclusive)
    pub(crate) fn overlaps(&self, other: &Span) -> bool {
        self.start <= other.end && other.start <= self.end
    }

    /// Merge two spans into one.
    /// The function assumes they overlap, or are adjacent.
    pub(crate) fn merge(&self, other: &Span) -> Self {
        Span {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
    }

    /// Get the length of the span in bytes.
    pub(crate) fn length(&self) -> usize {
        self.end.offset - self.start.offset
    }
}
