use std::fmt;

enum Color {
    Red,
    Green,
    Blue,
}

fn factorial(n: u64) -> u64 {
    if n == 0 {
        return 1;
    }
    n * factorial(n - 1)
}

fn main() {
    let msg = "hello";
    let x = 42;
    let b = true;
    println!("{}", factorial(x));
}
