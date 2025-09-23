const std = @import("std");
pub fn destructive(array: []i32, index: i32) i32 {
    const idx = @as(usize, @intCast(index + 2));
    return array[idx];
}

pub fn dangerous(array: []i32, index: i32) i32 {
    const idx = @as(usize, @intCast(index + 2));
    return array[idx];
}

pub fn foo(array: []i32, index: i32) i32 { return dangerous(array, index); }
pub fn foo1(array: []i32, index: i32) i32 { return foo(array, index * 3); }
pub fn foo2(array: []i32, index: i32) i32 { return foo1(array, index + 137); }
pub fn foo3(array: []i32, index: i32) i32 { return foo2(array, index - 1); }
pub fn foo4(array: []i32, index: i32) i32 { return foo3(array, index * 137); }
pub fn foo5(array: []i32, index: i32) i32 { return foo4(array, index + 20); }
pub fn foo6(array: []i32, index: i32) i32 { return foo5(array, @divTrunc(index, 3)); }
pub fn main() void {
    var a: [1000]i32 = .{0} ** 1000;
    _ = std.debug.print("{d}\n", .{ foo6(a[0..], 50) });
}

