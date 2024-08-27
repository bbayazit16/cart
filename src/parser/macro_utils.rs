/// Generates the consume function with given name and pattern.
/// Consume function expects the pattern to be matched. If the
/// given pattern does not match, the function returns a
/// `SyntaxError`.
#[macro_export]
macro_rules! generate_consume_impl {
    ($($name:ident => $pattern:pat, $expected:expr),+ $(,)?) => {
        impl $crate::parser::Parser {
            $(
                pub(super) fn $name(&mut self) -> Result<$crate::token::Token, CompileError> {
                    let token = self.peek()?;
                    match token.token_type {
                        $pattern => Ok(self.advance()),
                        _ => {
                            let e = CompileError::Syntax(SyntaxError::ExpectedDifferent {
                                span: token.span,
                                expected: $expected.to_string()
                            });
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
        impl $crate::parser::Parser {
            $(
                pub(super) fn $name(&mut self) -> bool {
                    let token = match self.peek() {
                        Ok(token) => token,
                        Err(_) => return false,
                    };
                    matches!(token.token_type, $pattern)
                }
            )+
        }
    };
}
