// Line comment
/* Block comment
   spanning lines */

//! Inner doc comment
/// Outer doc comment

#![allow(dead_code)]
#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

const MAX: usize = 100;
static NAME: &str = "edit";

fn numbers() {
    let _ = 42;
    let _ = 3.14;
    let _ = 0xff_u8;
    let _ = 0b1010;
    let _ = 0o77;
    let _ = 1_000_000i64;
    let _ = 1.5e-3;
}

fn strings() {
    let _ = "double \" quote \n escape";
    let _ = r"raw string \n no escape";
    let _ = r#"raw with "quotes" inside"#;
    let _ = b"byte string";
    let _ = 'a';
    let _ = '\n';
}

fn lifetimes<'a>(s: &'a str) -> &'a str {
    s
}

fn control(n: i32) -> Option<i32> {
    for i in 0..n {
        if i == 5 {
            continue;
        }
        while i < 10 {
            break;
        }
    }
    match n {
        0 => None,
        _ => Some(n),
    }
}

pub trait Greet {
    fn greet(&self) -> String;
}

impl Greet for Point {
    fn greet(&self) -> String {
        format!("({}, {})", self.x, self.y)
    }
}

fn main() {
    let p = Point { x: 1.0, y: 2.0 };
    println!("{}", p.greet());
    let v: Vec<i32> = vec![1, 2, 3];
    let b = true;
    let _ = if b { Ok(()) } else { Err(()) };
}
