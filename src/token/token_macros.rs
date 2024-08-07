//! This module defines the `Token` enum and associated methods,
//! used across the compiler.
//! 
//! Tokens are first generated in the lexer, via reading a source file.

/// A macro to define the `Token` enum with variants and associated methods.
///
/// This macro generates an enum `Token` with the specified variants. By default,
/// each token contains [`FilePointer`] in their first field.
/// 
/// To implement the display method for `Token`, see [`impl_token_display`]. To both define
/// and implement display at the same time, see the macro [`define_tokens_with_display`].
///
/// # Examples
/// ```rust
/// define_tokens! {
///     TokenVariant1(Type1, Type2, ...),
///     TokenVariant2(Type1),
///     TokenVariant3,
///     // ...
/// }
/// ```
///
#[macro_export]
macro_rules! define_tokens {
    ( $( $name:ident $( ( $( $tt:ty ),* ) )? ),* $(,)? ) => {
        #[derive(Debug, Clone)]
        #[allow(dead_code)]
        pub enum Token {
            $(
                $name($crate::context::FilePointer $( $(, $tt)* )?),
            )*
        }

        impl Token {
            /// Returns the file pointer of the token.
            pub fn get_file_pointer(&self) -> $crate::context::FilePointer {
                match self {
                    $(
                        Token::$name(pos, ..) => *pos,
                    )*
                }
            }

            /// Returns the line number of the token.
            pub fn get_line(&self) -> usize {
                self.get_file_pointer().line
            }

            /// Returns the file position of the token.
            pub fn get_file_position(&self) -> usize {
                self.get_file_pointer().file_position
            }

            /// Returns the line position of the token.
            pub fn get_line_position(&self) -> usize {
                self.get_file_pointer().line_position
            }
        }
    }
}

/// This macro implements `std::fmt::Display` for the `Token` enum to provide a string representation,
/// which omits [`FilePointer`] and prints rest of the fields, unlike the default debug
/// implementation which includes [`FilePointer`].
///
/// Either inner types or placeholders must be included in the macro.
/// # Examples
/// ```rust
/// impl_token_display! {
///     TokenVariant1(Type1, Type2, ...),
///     TokenVariant2(Type1),
///     TokenVariant3,
///     // ...
/// }
/// ```
#[macro_export]
macro_rules! impl_token_display {
    ($($name:ident $(($($field:ident),+))?),+ $(,)?) => {
        impl std::fmt::Display for Token {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Token::$name(_, $($($field),+)?) => {
                            write!(f, "{}", stringify!($name))?;
                            $(
                                write!(f, "(")?;
                                $(
                                    write!(f, "{}", $field)?;
                                    write!(f, ", ")?;
                                )+
                                write!(f, ")")?;
                            )?
                        }
                    ),+
                }
                Ok(())
            }
        }
    };
}

/// This macro both defines, via [`define_tokens`] and implement display,
/// via [`impl_token_display`] at the same time. To do these actions separately,
/// use these macros individually.
/// 
/// # Examples
/// ```rust
/// define_tokens_with_display! {
///     TokenVariant1(Type1, Type2, ...),
///     TokenVariant2(Type1),
///     TokenVariant3,
///     // ...
/// }
/// ```
#[macro_export]
macro_rules! define_tokens_with_display {
    ( $( $name:ident $( ($($inner:tt)*) )? ),* $(,)? ) => {
        define_tokens! {
            $(
                $name $( ($($inner)*) )?,
            )*
        }

        impl_token_display! {
            $(
                $name $( ($($inner)*) )?,
            )*
        }
    }
}