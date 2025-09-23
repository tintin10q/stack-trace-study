const std = @import("std");

fn dangerous(a: []i32, index: usize) i32 {
    return a[index]; // safety-checked in Debug
}

fn foo(a: []i32, counter: i32) i32 {
    if (counter == 0) return dangerous(a, @intCast(counter + 9137));
    return foo(a, counter - 1);
}

pub fn main() void {
    var arr: [1000]i32 = [_]i32{0} ** 1000;
    const r = foo(arr[0..], 6);
    std.debug.print("The result is {d}\n", .{r});
}
