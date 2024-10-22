//! The Cart standard library is included with every Cart program.
//! It is linked to the program after compilation.
use crate::cart_array::CartArray;
use va_list::VaList;

mod cart_array;

/// `CartStringRepr` is a C repr of the `CartString` struct used in the standard library.
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct CartStringRepr {
    pub ref_count: i64,
    pub length: i64,
    pub data: *const u8,
}

/// Print a number to the standard output.
#[no_mangle]
pub extern "C" fn print_number(n: i32) {
    println!("{}", n);
}

/// Prints a string to the standard output.
///
/// # Safety
///
/// This function is marked unsafe because it dereferences a raw pointer. If null,
/// a scope error must have already occurred.
#[no_mangle]
pub unsafe extern "C" fn print_string(s: *const CartStringRepr) {
    if s.is_null() {
        panic!("Received null pointer in print_string");
    }

    let s = unsafe { &*s };
    let bytes = unsafe { std::slice::from_raw_parts(s.data, s.length as usize) };
    let rust_string = std::str::from_utf8(bytes).expect("Invalid UTF-8 string");

    println!("{}", rust_string);
}

/// Formats and prints a string to the standard output.
/// The function takes variadic arguments, and formats the string with the arguments provided.
///
/// # Safety
/// This function is marked unsafe because it dereferences a raw pointer.
/// The pointer is assumed to be valid as it is passed from Cart code.
#[no_mangle]
pub unsafe extern "C" fn unstable_print(s: *const CartStringRepr, mut args: VaList) {
    if s.is_null() {
        panic!("Received null pointer in println");
    }

    let s = unsafe { &*s };
    let bytes = unsafe { std::slice::from_raw_parts(s.data, s.length as usize) };
    let rust_string = std::str::from_utf8(bytes).expect("Invalid UTF-8 string");

    // Now, rust_string may include format specifiers {}.
    // Those have to be replaced with args in the variadic arguments.
    let mut formatted_string = rust_string.to_string();
    while formatted_string.contains("{}") {
        let arg = args.get::<i32>();
        formatted_string = formatted_string.replace("{}", &arg.to_string());
    }

    println!("{}", formatted_string);
}

#[no_mangle]
/// Creates a new `CartArray` with the given initial capacity.
pub extern "C" fn create_array(initial_capacity: u32) -> *mut CartArray<i32> {
    Box::into_raw(Box::new(CartArray::<i32>::new(initial_capacity as usize)))
}

#[no_mangle]
/// Pushes a value to the `CartArray`.
///
/// # Safety
/// Assumes that the given array pointer is not null and points to a valid `CartArray`.
/// This is enforced by the compiler's type checking.
pub unsafe extern "C" fn push_to_array(array: *mut CartArray<i32>, value: i32) {
    if array.is_null() {
        panic!("Received null pointer in push_to_array");
    }

    let array = unsafe { &mut *array };
    array.push(value);
}

#[no_mangle]
/// Pushes multiple values to the `CartArray`.
///
/// # Safety
/// This function has the same safety requirements as `push_to_array`.
/// It assumes that the given array pointer is not null and points to a valid `CartArray`.
/// This is enforced by the compiler's type checking.
pub unsafe extern "C" fn push_to_array_multiple(
    array_ptr: *mut CartArray<i32>,
    values_ptr: *const i32,
    count: u32,
) {
    let array = unsafe { &mut *array_ptr };
    let values = unsafe { std::slice::from_raw_parts(values_ptr, count as usize) };
    array.push_multiple(values);
}
