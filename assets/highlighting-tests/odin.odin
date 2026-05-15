// Single-line comment

/*
 * Multi-line
 * comment
 */

#+feature dynamic-literals
package main

import "core:fmt"

// Package-level constants
MY_CONST :: 42
PI       :: 3.14159
NAME     :: "Odin"

// Struct type
Vector3 :: struct {
    x, y, z: f32,
}

// Union type
Value :: union {
    int,
    f64,
    string,
}

// Enum type
Direction :: enum {
    North,
    South,
    East,
    West,
}

// Bit set
Flags :: bit_set[Direction]

// Distinct type
Meters :: distinct f32

// Procedures
add :: proc(a, b: int) -> int {
    return a + b
}

variadic :: proc(nums: ..int) -> int {
    result := 0
    for n in nums {
        result += n
    }
    return result
}

get_value :: proc() -> (int, bool) {
    return 42, true
}

@(deprecated = "use new_func instead")
old_func :: proc() {
    fmt.println("old")
}

demo_numbers :: proc() {
    // Integer and float literals
    _ := 42
    _ := 3.14
    _ := 1_000_000
    _ := 0xFF
    _ := 0b1010
    _ := 0o77
    _ := 1.5e-3
    _ := 2i          // imaginary

    // Language constants
    _ := true
    _ := false
    _ = nil
}

demo_strings :: proc() {
    // Double-quoted strings with escape sequences
    _ := "hello, world"
    _ := "escape: \" \n \t \\"

    // Raw strings (backtick — no escape processing)
    _ := `C:\Windows\notepad.exe`
    _ := `no \n escapes here`

    // Rune / character literals
    _ := 'A'
    _ := '\n'
    _ := '世'
}

demo_control_flow :: proc() {
    // If / else if / else
    if true {
        fmt.println("yes")
    } else if false {
        fmt.println("no")
    } else {
        fmt.println("maybe")
    }

    // C-style for loop
    for i := 0; i < 10; i += 1 {
        if i == 5 { continue }
        if i == 8 { break }
    }

    // Range-based for loop
    for i in 0..<10 {
        fmt.println(i)
    }

    // Switch statement
    x := 42
    switch x {
    case 1:
        fmt.println("one")
    case 2, 3:
        fmt.println("two or three")
    case:
        fmt.println("other")
    }

    // Fallthrough
    switch 0 {
    case 0:
        fallthrough
    case 1:
        fmt.println("zero or one")
    }

    // When (compile-time conditional)
    when ODIN_OS == .Windows {
        fmt.println("Windows")
    } else {
        fmt.println("other OS")
    }

    // Defer
    defer fmt.println("deferred")

    // or_else / or_break
    val := get_value() or_else 0
    _ = val

    for {
        get_value() or_break
    }
}

demo_types :: proc() {
    // Struct literal
    v := Vector3{x = 1, y = 4, z = 9}
    fmt.println(v)

    // Union
    val: Value = 137
    if i, ok := val.(int); ok {
        fmt.println(i)
    }

    // Enum and switch
    d := Direction.North
    switch d {
    case .North:
        fmt.println("north")
    case .South, .East, .West:
        fmt.println("other direction")
    }

    // Bit set
    flags := Flags{.North, .East}
    fmt.println(.North in flags)

    // Distinct type
    dist := Meters(100.0)
    fmt.println(dist)

    // Cast / transmute / auto_cast
    x: int = cast(int)3.14
    y := transmute([4]u8)u32(0xDEAD_BEEF)
    z := auto_cast x
    _, _ = y, z
}

demo_collections :: proc() {
    // Dynamic array
    arr := make([dynamic]int)
    defer delete(arr)
    append(&arr, 1, 2, 3)
    fmt.println(arr)

    // Map
    m := make(map[string]int)
    defer delete(m)
    m["key"] = 99
    fmt.println(m["key"])

    // Pointer
    val := 123
    ptr := &val
    ptr^ = 456
    fmt.println(val)
}

demo_builtins :: proc() {
    // typeid / type_of
    _ = typeid_of(int)
    val := 0
    _ = type_of(val)

    // size_of / align_of / offset_of
    _ = size_of(Vector3)
    _ = align_of(f64)
    _ = offset_of(Vector3, y)

    // context
    context.user_index = 1
}

demo_directives :: proc() {
    // Compile-time assert
    #assert(size_of(u8) == 1)

    // SOA layout
    v: #soa[4]Vector3
    _ = v

    // Partial switch
    #partial switch d := Direction.North; d {
    case .North:
        fmt.println("north")
    }

    // Unroll for
    #unroll for elem, idx in [3]int{1, 4, 9} {
        fmt.println(elem, idx)
    }
}

main :: proc() {
    demo_numbers()
    demo_strings()
    demo_control_flow()
    demo_types()
    demo_collections()
    demo_builtins()
    demo_directives()

    fmt.println(add(1, 2))
    fmt.println(variadic(1, 2, 3, 4, 5))
    old_func()
}
