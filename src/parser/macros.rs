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
                    let token = self.advance();
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

/// Used to handle the result of a function call that returns a `Result<Token, CompileError>`.
/// It checks if the function call is successful or if it results in an error.
/// If an error occurs, the macro pushes the error to the errors vector, and synchronizes
/// the parser position.
#[macro_export]
macro_rules! report {
    ($self:expr, $expr:expr) => {{
        let before = $self.peek()?.get_file_pointer();
        match $expr {
            Ok(val) => val,
            Err(e) => {
                $self.errors.push(e);
                $self.recover_to_position(before)?;
                $self.synchronize();
                return Ok(ast::NotRecovered::not_recovered());
            }
        }
    }};
}

    // if let Err(ref e) = $e {
    //     $self.errors.push(e);
    //     $self.synchronize();
    // }
    // ($self:expr, $e:expr, $default_token:expr) => {
    //     match $e {
    //         Ok(token) => token,
    //         Err(ref e) => {
    //             $self.report(e);
    //             $default_token
    //         }
    //     }
    // };
// 
// /// Similar to report, but returns the token, and exits the function
// /// if the token can't be returned.
// #[macro_export]
// macro_rules! report_and_return {
//     ($self:expr, $e:expr) => {
//         match $e {
//             Ok(token) => token,
//             Err(ref e) => {
//                 $self.report(e);
//                 return ast::NotRecovered::not_recovered();
//             }
//         }
//     }
// }

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