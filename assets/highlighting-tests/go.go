// Line comment
/* Block comment
   spanning lines */

package main

import (
	"fmt"
	"strings"
)

const Pi = 3.14159

type Point struct {
	X, Y float64
}

func numbers() {
	_ = 42
	_ = 0xff
	_ = 0b1010
	_ = 0o77
	_ = 1_000_000
	_ = 1.5e-3
	_ = 3.14i
}

func stringLiterals() {
	_ = "double \" quote \n escape"
	_ = `raw string
	spanning lines \n no escape`
	_ = 'a'
	_ = '\n'
}

func control(n int) int {
	for i := 0; i < n; i++ {
		if i == 5 {
			continue
		}
		switch i {
		case 1:
			break
		default:
			fallthrough
		}
	}
	return n
}

func (p Point) String() string {
	return fmt.Sprintf("(%v, %v)", p.X, p.Y)
}

func main() {
	p := Point{X: 1.0, Y: 2.0}
	var b bool = true
	var s string = strings.ToUpper("hi")
	fmt.Println(p.String(), b, s, nil)
}
