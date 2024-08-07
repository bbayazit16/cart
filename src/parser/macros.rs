//! Macros module defines macros that automatically implement
//! matching for the `Parser`.
//! 
//! These macros are used in parsing to implement
//! certain functions that match certain Tokens
//! outputted by the Lexer.
//! 
//! The macros also provide default error recovery,
//! and report errors to the [`Reporter`].

/// Generates the consume function with given name and pattern.
/// Consume function expects the pattern to be matched. If the
/// given pattern does not match, the function returns a
/// `SyntaxError`.
#[macro_export]
macro_rules! generate_consume_impl {
    ($($name:ident => $pattern:pat, $expected:expr),+ $(,)?) => {
        use $crate::errors::SyntaxError;
        impl Parser {
            $(
                fn $name(&mut self) -> Result<Token, CompileError> {
                    let token = self.advance()?;
                    match token {
                        $pattern => Ok(token),
                        _ => {
                            let e = CompileError::Syntax(SyntaxError::ExpectedDifferentCharacter {
                                file_pointer: token.get_file_pointer(),
                                expected: $expected.to_string()
                            });
                            // self.report(&e);
                            Err(e)
                        }
                    }
                }
            )+
        }
    };
}

/// Generates the match function with given name and pattern.
/// Unlike the consume function, match function is not as strict.
/// It expects the pattern to be matched, but if not, returns false
/// without throwing any errors. 
#[macro_export]
macro_rules! generate_match_impl {
    ($($name:ident => $pattern:pat),+ $(,)?) => {
        impl Parser {
            $(
                fn $name(&mut self) -> bool {
                    let token = match self.peek() {
                        Ok(token) => token,
                        Err(_) => return false,
                    };
                    matches!(token, $pattern)
                }
            )+
        }
    };
}