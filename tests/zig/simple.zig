const std = @import("std");

const Color = enum {
    red,
    green,
    blue,
};

fn factorial(n: u64) u64 {
    if (n == 0) return 1;
    return n * factorial(n - 1);
}

const Point = struct {
    x: f64,
    y: f64,
};

pub fn main() void {
    const result = factorial(42);
    const msg = "hello";
    const flag = true;
    _ = result;
    _ = msg;
    _ = flag;
}
