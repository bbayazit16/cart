use inkwell::types::BasicTypeEnum;

/// `CartType` wraps around Inkwell `BasicTypeEnum` and allows extra information
/// to be assigned to the types. It employs a builder-like pattern.
///
/// Implements `Clone`, `Debug`, and `PartialEq` traits.
///
/// # Fields
/// - `type_enum`: The `BasicTypeEnum` that the `CartType` wraps around.
/// - `type_name`: An optional `String` that represents the name of the type.
/// - `is_alloca`: A `bool` that represents whether the type is an alloca.
#[derive(Debug, Clone)]
pub(super) struct CartType<'ctx> {
    pub type_enum: BasicTypeEnum<'ctx>,
    pub type_name: Option<String>,
    pub is_alloca: bool,
}

impl<'ctx> CartType<'ctx> {
    /// Creates a new `CartType` from the given `BasicTypeEnum` and type name.
    #[allow(unused)]
    pub(super) fn new(type_enum: BasicTypeEnum<'ctx>, type_name: String, is_alloca: bool) -> Self {
        Self {
            type_enum,
            type_name: Some(type_name),
            is_alloca,
        }
    }

    /// Return the type name of the `CartType`.
    pub(super) fn name(&self) -> Option<&str> {
        self.type_name.as_deref()
    }

    /// Set the type name of the `CartType` and return the `CartType`.
    pub(super) fn with_name(mut self, type_name: String) -> Self {
        self.type_name = Some(type_name);
        self
    }

    /// Remove the type name of the `CartType` and return the `CartType`.
    #[allow(unused)]
    pub(super) fn without_name(mut self) -> Self {
        self.type_name = None;
        self
    }

    /// Set the `is_alloca` of the `CartType` and return the `CartType`.
    pub(super) fn with_alloca(mut self) -> Self {
        self.is_alloca = true;
        self
    }

    /// Set the `is_alloca` of the `CartType` to false and return the `CartType`.
    #[allow(unused)]
    pub(super) fn without_alloca(mut self) -> Self {
        self.is_alloca = false;
        self
    }
}

impl<'ctx> From<CartType<'ctx>> for BasicTypeEnum<'ctx> {
    fn from(cart_type: CartType<'ctx>) -> Self {
        cart_type.type_enum
    }
}

impl<'ctx> From<BasicTypeEnum<'ctx>> for CartType<'ctx> {
    fn from(basic_type_enum: BasicTypeEnum<'ctx>) -> Self {
        Self {
            type_enum: basic_type_enum,
            type_name: None,
            is_alloca: false,
        }
    }
}

impl<'ctx> PartialEq for CartType<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.type_enum == other.type_enum
    }
}
