use thiserror::Error;

#[derive(Error, Debug)]
pub enum SyntaxError {
    // $0 = file, $1 = line
    #[error("unterminated string literal")]
    UnterminatedStringLiteral(String, usize),
}
//
// #[derive(Error, Debug)]
// pub enum SysError {
//     // $0 = file
//     #[error("unable to read from file '{0}'")]
//     IOError(String)
// }
// #[derive(Error, Debug)]
// pub struct IOError {
//     pub(crate) source: std::io::Error,
//     pub(crate) file_name: String,
// }

#[derive(Error, Debug)]
pub enum LexerError {
    #[error(transparent)]
    Syntax(#[from] SyntaxError),

    #[error(transparent)]
    IO(#[from] std::io::Error),
}
