// Line comment
/*
 * Multi-line
 * comment
 */

#+feature dynamic-literals
package main

import "core:fmt"

// Numbers
Dec  :: 42
Neg  :: -7
Hex  :: 0xCAFE_BABE
Oct  :: 0o755
Bin  :: 0b1010
Sep  :: 1_000_000
Flt  :: 3.14
Half :: .5
Exp  :: 1e10
Sci  :: 1.5e-3
Imag :: 2i
Quat :: 3j
HexF :: 0h3f800000
Bad  :: 1abc

// Constants
yes  := true
no   := false
none := nil
_     = 0

// Basic types
b:    bool
b32v: b32
r:    rune
s:    string
cs:   cstring
i:    int
i128v: i128
u:    uint
up:   uintptr
u32b: u32be
i16l: i16le
f16v: f16
f64v: f64
c128: complex128
q256: quaternion256
p:    rawptr
t:    typeid
a:    any

// Strings and runes
char    := 'a'
escaped := '\n'
quote   := '\''
str     := "double quotes with escapes: \" \n \t \\"
raw     := `raw string
spans lines and may contain "quotes"`

Speaker :: struct #packed {
	name: string `fmt:"q"`,
}

Value :: union {int, f64}

Direction :: enum {North, South}

Flags :: bit_set[Direction]

Meters :: distinct f32

Header :: bit_field u32 {
	tag: u8 | 3,
}

@(deprecated = "use speak instead")
old_speak :: proc(s: Speaker) -> string {
	return fmt.tprintf("%s speaks", s.name)
}

@private
compare :: proc(a, b: $T) -> bool where intrinsics.type_is_comparable(T) {
	return a == b
}

foreign import kernel32 "system:kernel32.lib"

@(default_calling_convention = "std")
foreign kernel32 {
	ExitProcess :: proc(code: u32) ---
}

main :: proc() {
	if yes {
		fmt.println("enabled")
	} else when ODIN_DEBUG {
		fmt.println("debug")
	} else {
		fmt.println("disabled")
	}

	for i := 0; i < 10; i += 1 {
		switch {
		case i == 5:
			continue
		case i == 8:
			break
		case:
			fallthrough
		}
	}

	for i in 0..<10 {}
	for i in 0..=9 {}

	values := [dynamic]int{1, 2, 3}
	m := map[string]int{"one" = 1}
	defer delete(values)
	for value, index in values {
		fmt.println(index, value, m["one"])
	}

	v: Value = 137
	#partial switch _ in v {
	case int:
		fmt.println("int")
	}

	soa: #soa[4]Speaker
	#unroll for i in 0..<2 {}

	x := cast(int)3.14
	y := transmute(u32)f32(1)
	z := auto_cast x
	w := m["two"] or_else 0
	_, _, _, _ = x, y, z, w

	#assert(size_of(u8) == 1)
	context.user_index = 1
	ptr := &x
	ptr^ = 456
}
