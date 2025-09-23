const std = @import("std");
fn dangerous(v1:i32, v2:i32) i32 { return @divTrunc(v1, v2); }
fn foo(array: []i32, counter:i32) i32 {
  if (counter==0) return dangerous(array[0], counter);
  return foo(array, counter-1);
}
pub fn main() void {
  var a:[1000]i32 = .{0} ** 1000;
  _ = std.debug.print("The result is {d}\n", .{ foo(a[0..], 6) });
}

