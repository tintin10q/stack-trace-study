package main

import "fmt"

func dangerous(a []int, index int) int {
	return a[index] // panic: index out of range
}

func foo(a []int, counter int) int {
	if counter == 0 {
		return dangerous(a, counter+9137)
	}
	return foo(a, counter-1)
}

func main() {
	a := make([]int, 1000)
	r := foo(a, 900)
	fmt.Printf("The result is %d\n", r)
}
