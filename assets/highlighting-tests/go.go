// Comments
// Single-line comment

/*
 * Multi-line
 * comment
 */

package main

import (
    "fmt"
    "time"
)

const Pi = 3.14
var enabled = true

// Numbers
42
3.14
.5
1e10
1.5e-3
0xff
0XFF
0b1010
0o77
1_000_000
2.5i

// Constants
true
false
nil
iota

// Strings and runes
'a'
'\n'
"double quotes with escape: \" \n \t \\"
`raw string
spans lines`

// Control flow
func main() {
    if enabled {
        fmt.Println("enabled")
    } else {
        fmt.Println("disabled")
    }

    for i := 0; i < 10; i++ {
        if i == 5 {
            continue
        }
        if i == 8 {
            break
        }
    }

    switch now := time.Now().Weekday(); now {
    case time.Saturday, time.Sunday:
        fmt.Println("weekend")
    default:
        fmt.Println("weekday")
    }

    values := []int{1, 2, 3}
    for _, value := range values {
        fmt.Println(value)
    }

    ch := make(chan int, 1)
    go func() {
        defer close(ch)
        ch <- 42
    }()

    select {
    case msg := <-ch:
        fmt.Println(msg)
    default:
        fmt.Println("no message")
    }
}

type Speaker interface {
    Speak() string
}

type Animal struct {
    Name string
}

func (a Animal) Speak() string {
    return fmt.Sprintf("%s speaks", a.Name)
}

func greet(name string) string {
    return fmt.Sprintf("hello %s", name)
}

var _ Speaker = Animal{}

// Function calls
fmt.Println(greet("world"))
time.Now()
