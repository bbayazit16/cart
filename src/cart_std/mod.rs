//! The Cart standard library is included with every Cart program.
//! It is linked to the program at compile time.

#[no_mangle]
pub extern "C" fn println(n: i32) {
    println!("{}", n);
}
