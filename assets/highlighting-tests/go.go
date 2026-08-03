// Line comment
/*
 * Multi-line
 * comment
 */

package main

import (
	"fmt"
	"time"
)

// Numbers
const (
	Dec  = 42
	Neg  = -7
	Hex  = 0xCAFE_BABE
	Oct  = 0o755
	Old  = 0755
	Bin  = 0b1010
	Sep  = 1_000_000
	Flt  = 3.14
	Half = .5
	Exp  = 1e10
	Sci  = 1.5e-3
	Imag = 2.5i
)

// Constants and iota
const (
	A = iota
	B
)

var (
	yes  = true
	no   = false
	none = nil
)

// Predeclared types
var (
	b    bool
	by   byte
	r    rune
	s    string
	i    int
	i8   int8
	i64  int64
	u    uint
	u32  uint32
	up   uintptr
	f64  float64
	c128 complex128
	e    error
	a    any
)

// Strings and runes
var (
	char    = 'a'
	escaped = '\n'
	quote   = '\''
	str     = "double quotes with escapes: \" \n \t \\"
	raw     = `raw string
spans lines and may contain "quotes"`
)

type Speaker interface {
	Speak() string
}

type Animal struct {
	Name string `json:"name"`
}

func (a Animal) Speak() string {
	return fmt.Sprintf("%s speaks", a.Name)
}

func compare[T comparable](a, b T) bool {
	return a == b
}

func main() {
	if yes {
		fmt.Println("enabled")
	} else {
		fmt.Println("disabled")
	}

	for i := 0; i < 10; i++ {
		switch {
		case i == 5:
			continue
		case i == 8:
			goto done
		default:
			fallthrough
		}
	}

	switch now := time.Now().Weekday(); now {
	case time.Saturday, time.Sunday:
		fmt.Println("weekend")
	}

	values := []int{1, 2, 3}
	m := map[string]int{"one": 1}
	for _, value := range values {
		fmt.Println(value, m)
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

done:
	var _ Speaker = Animal{Name: "dog"}
}
