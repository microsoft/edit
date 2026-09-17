// Comments
// Line comment
/* Block comment */
/*
 * Block comment multiple lines
 */
/// Outer line doc comment
/** Outer block doc comment */
/**
 * Outer block doc comment multiple lines
 */
//! Inner line doc comment
/*! Inner block doc comment */
/*!
 * Inner block doc comment multiple lines
 */

// Numbers
123;
123i32;
123u32;
123_u32;

0xFF;
0xff_u8;
0x01_f32;
0x01_e3;

0o70;
0o70_i16;

0b1111_1111_1001_0000;
0b1111_1111_1001_0000i64;
0b________1;

0usize;

128_i8;
256_u8;

5f32;

123.0f64;
0.1f64;
0.1f32;
12E+99_f64;
let x: f64 = 2.;

// Constants
true;
false;

// Strings
'H'
'\''
'\xac'
'\u{AC00}'
"hello \' \" \n \\ \xac \u{AC00}"
r#"hello"#
b'H'
b'\''
b"hello \' \" \n \\ \xac"
br#"hello"#
c"hello \' \" \n \\ \xac \u{AC00}"
cr#"hello"#

// Control flow keywords
if true {
} else if false {
} else {
}

for i in 0..10 {
    if i == 5 { continue; }
    if i == 8 { break; }
}

while false { }
loop { break; }

match 42 {
    1 => (),
    2 | 3 => (),
    4..=10 => (),
    _ => (),
}

// Variables and bindings
let a = 1;
let mut b = 2;
const PI: f64 = 3.14;
static COUNT: u32 = 0;

// Functions
fn greet(name: &str) -> String {
    return format!("Hello, {}", name);
}

async fn fetch_data() -> Result<String, std::io::Error> {
    let result = read_remote().await?;
    Ok(result)
}

// Structs, impls, methods
pub struct Animal {
    pub name: String,
    count: u32,
}

impl Animal {
    pub fn new(name: String) -> Self {
        Self { name, count: 0 }
    }

    pub fn speak(&self) -> String {
        format!("{} speaks", self.name)
    }
}

// Traits and dynamic dispatch
pub trait Speak {
    fn speak(&self) -> String;
    fn name(&self) -> &str where Self: Sized;
}

impl Speak for Animal {
    fn speak(&self) -> String {
        format!("{} speaks", self.name)
    }

    fn name(&self) -> &str {
        &self.name
    }
}

let animal: Box<dyn Speak> = Box::new(Animal::new("Rex".to_string()));

// Enums and pattern matching
pub enum Direction {
    North,
    South,
    East,
    West,
}

match Direction::North {
    Direction::North | Direction::South => (),
    other => drop(other),
}

// Modules, imports, re-exports
mod submodule {
    pub use super::Animal;

    pub fn helper() { }
}

use std::collections::HashMap;
use std::io::{self, Read, Write};

extern crate serde;

// Type aliases and unsafe
type Callback = fn(i32) -> i32;

unsafe fn dangerous() {
    let raw: *const i32 = &42;
    let _value = *raw;
}

// Generics, lifetimes, where clauses
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}

struct Wrapper<T: Clone> where T: std::fmt::Debug {
    value: T,
}

impl<T: Clone + std::fmt::Debug> Wrapper<T> {
    fn get(&self) -> T {
        self.value.clone()
    }
}

// Closures, references, moves
let add = |a: i32, b: i32| a + b;
let doubled: Vec<i32> = vec![1, 2, 3].iter().map(|x| x * 2).collect();

let s = String::from("hello");
let closure = move || println!("{}", s);

let ref r = 42;
let mut v = vec![1, 2, 3];
v.push(4);

// Raw identifiers and casts
let r#type = "raw";
r#fn();
let n = 42 as f64;
let flag = true as u8;

// Macro and function calls
println!("hello, world!");
format!("{}: {}", 1, 2);
vec![1, 2, 3];
dbg!(add(1, 2));
greet("world");
HashMap::<String, i32>::new();
