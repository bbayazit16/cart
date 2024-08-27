//! Context module defines `Context` - a struct passed across the
//! lexer, parser, and many other structs needed to parse source
//! code. Context module also defines `FilePointer` - which provides
//! abstraction  for the token positions. `FilePointer` struct represents
//! a specific position in source code. It provides abstraction for the Lexer
//! and Tokens - allowing reverts to the given `FilePointer`.
mod file_context;
mod span;

pub(crate) use file_context::FileContext;
pub(crate) use span::{Position, Span};
