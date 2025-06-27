// Test Go file for syntax highlighting and tabs
package main

import (
    "fmt"
    "strings"
)

func main() {
    message := "Hello, World!"
    upperMessage := strings.ToUpper(message)
    
    for i := 0; i < 5; i++ {
        fmt.Printf("Iteration %d: %s\n", i+1, upperMessage)
    }
}

type Person struct {
    Name string
    Age  int
}

func (p Person) Greet() {
    fmt.Printf("Hello, my name is %s and I'm %d years old\n", p.Name, p.Age)
}
