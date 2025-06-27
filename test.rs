// This is a test Rust file for syntax highlighting
use std::collections::HashMap;

fn main() {
    let mut map: HashMap<String, i32> = HashMap::new();
    map.insert("hello".to_string(), 42);
    
    if let Some(value) = map.get("hello") {
        println!("Value: {}", value);
    }
    
    for i in 0..10 {
        println!("Number: {}", i);
    }
}

struct Person {
    name: String,
    age: u32,
}

impl Person {
    fn new(name: String, age: u32) -> Self {
        Person { name, age }
    }
}
