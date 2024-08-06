//! Context module defines `Context` - a struct passed across the
//! lexer, parser, and many other structs needed to parse source
//! code.
use crate::reporter::Reporter;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};

/// `FileContext` struct holds shared information - such as a bufreader
/// to read the shared file, the `Reporter` to report the errors,
/// or the name of the current file being parsed.
///
/// # Fields
/// - `reader`: A buffered reader for reading the input source code.
/// - `reporter`: The reporter used to report errors in this file.
/// - `file_path`: The original path to the file.
#[derive(Debug)]
pub struct FileContext {
    reader: BufReader<File>,
    reporter: Reporter,
    file_path: PathBuf,
}

impl FileContext {
    /// Initialize a new `FileContext`.
    /// If the file can't be opened, report via the reporter,
    /// and return None.
    pub fn try_new<P: AsRef<Path>>(file_path: &P) -> Option<Self> {
        match Self::safe_open(&file_path) {
            Some(file) => Some(FileContext {
                reader: BufReader::new(file),
                reporter: Reporter::new(file_path),
                file_path: file_path.as_ref().to_path_buf(),
            }),
            None => None,
        }
    }

    /// Returns a reference to the reader.
    /// Allows the file to be read from the context.
    pub fn reader(&mut self) -> &mut BufReader<File> {
        &mut self.reader
    }

    /// Returns a reference to the file path.
    pub fn file_path(&self) -> &PathBuf {
        &self.file_path
    }
    
    /// Returns a reference to the reporter.
    pub fn reporter(&self) -> &Reporter {
        &self.reporter
    }

    /// Safely open the file.
    /// If the file can't be opened, report to the reporter
    /// and return None.
    fn safe_open<P: AsRef<Path>>(file_path: &P) -> Option<File> {
        let file = File::open(file_path);
        match file {
            Ok(opened_file) => Some(opened_file),
            Err(ref io_err) => {
                Reporter::report_io_error::<&str>(io_err, None);
                None
            }
        }
    }
}
