/// CartString is a simple string type used in the Cart language.
/// CartString is heap-allocated, and contains a pointer to the string data.
///
/// # String Layout
/// - The first 32 bits indicate the reference count of the string.
/// - The next 32 bits indicate the length of the string.
/// - The last 8 bits indicate a pointer to the string data.
#[repr(C)]
pub struct CartString {
    pub reference_count: i32,
    pub length: i32,
    pub data: *const u8,
}

impl From<&str> for CartString {
    fn from(s: &str) -> Self {
        let data = s.as_bytes().as_ptr();
        let length = s.len() as i32;
        Self {
            reference_count: 1,
            length,
            data,
        }
    }
}
