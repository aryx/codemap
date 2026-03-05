package main

import "fmt"

type Color int

const (
	Red   Color = 0
	Green Color = 1
	Blue  Color = 2
)

func factorial(n int) int {
	if n == 0 {
		return 1
	}
	return n * factorial(n-1)
}

func main() {
	msg := "hello"
	x := 42
	fmt.Println(msg, factorial(x))
}
