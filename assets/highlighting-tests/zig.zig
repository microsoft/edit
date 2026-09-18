// Single-line comment
/// Documentation comment.

// Imports
const std = @import("std");
const builtin = @import("builtin");

// Constants and variables
const answer: i32 = 42;
const pi = 3.14159;
var counter: usize = 0;
pub const name = "Zig";

// Integer literals
const decimal = 123;
const binary = 0b1010_0101;
const octal = 0o755;
const hex = 0xDEAD_BEEF;
const separated = 1_000_000;

// Floating-point literals
const float = 3.14;
const exponent = 1.5e-3;
const hex_float = 0x1.8p1;

// Character and string literals
const char = 'a';
const newline = '\n';
const unicode = '\u{1F600}';
const string = "escapes: \" \\ \n \t";

// Multiline string
const multiline =
    \\first line
    \\second line
;

// Identifiers
const @"quoted identifier" = 123;
const snake_case = true;

// Operators
const arithmetic = 1 + 2 * 3 - 4 / 2 % 2;
const comparison = a == b and c != d or e >= f;
const bitwise = x & y | z ^ w;
const shifts = value << 2 >> 1;
const wrapping = a +% b;
const saturating = a +| b;

// Struct
const Point = struct {
    x: i32,
    y: i32,

    pub fn init(x: i32, y: i32) Point {
        return .{ .x = x, .y = y };
    }
};

const origin = Point{
    .x = 0,
    .y = 0,
};

// Enum
const Color = enum {
    red,
    green,
    blue,
};

// Tagged union
const Value = union(enum) {
    integer: i64,
    float: f64,
    boolean: bool,
};

// Arrays, slices, tuples
const array = [_]u8{ 1, 2, 3, 4 };
const slice = array[1..3];
const tuple = .{ 42, true, "hello" };

// Pointers
var value: i32 = 42;
const ptr: *i32 = &value;
const many_ptr: [*]i32 = undefined;
const sentinel_ptr: [*:0]const u8 = undefined;

// Functions
fn add(a: i32, b: i32) i32 {
    return a + b;
}

fn generic(comptime T: type, value: T) T {
    return value;
}

// If / else
fn classify(x: i32) i32 {
    if (x > 10) {
        return 1;
    } else if (x == 10) {
        return 0;
    } else {
        return -1;
    }
}

// While / for / labels
fn loops() void {
    var i: usize = 0;

    while (i < 10) : (i += 1) {
        if (i == 5) continue;
        if (i == 8) break;
    }

    for (array, 0..) |item, index| {
        _ = item;
        _ = index;
    }

    outer: for (array) |item| {
        if (item == 3) break :outer;
    }
}

// Switch
fn describe(value: Value) void {
    switch (value) {
        .integer => |x| _ = x,
        .float => |x| _ = x,
        .boolean => |x| _ = x,
    }
}

// Optionals
fn optional(value: ?i32) i32 {
    return value orelse 0;
}

// Errors
const MyError = error{
    NotFound,
    InvalidValue,
};

fn mightFail() MyError!i32 {
    return error.NotFound;
}

fn handleError() !void {
    const result = try mightFail();
    _ = result;

    _ = mightFail() catch 42;
}

// Defer
fn cleanup() void {
    defer std.debug.print("done\n", .{});
}

// Comptime / builtins
const type_of_value = @TypeOf(value);
const size_of_point = @sizeOf(Point);
const alignment = @alignOf(Point);
const field_name = @tagName(Color.red);

// Builtin-heavy expression
const converted = @intCast(@as(u64, 42));
const bits = @bitCast(@as(u32, 0x3F800000));

// Extern / export / calling convention
extern fn external_function(x: i32) i32;

export fn exported_function(x: i32) callconv(.c) i32 {
    return x;
}

// Tests
test "basic arithmetic" {
    try std.testing.expect(add(1, 2) == 3);
}

test "optional" {
    try std.testing.expect(optional(null) == 0);
}
