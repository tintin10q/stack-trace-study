package main
import "fmt"

func dangerous(array []int, index int) int { return array[index+2] }
func foo(array []int, index int) int { return dangerous(array, index) }
func foo1(array []int, index int) int { return foo(array, index*3) }
func foo2(array []int, index int) int { return foo1(array, index+137) }
func foo3(array []int, index int) int { return foo2(array, index-1) }
func foo4(array []int, index int) int { return foo3(array, index*137) }
func foo5(array []int, index int) int { return foo4(array, index+20) }
func foo6(array []int, index int) int { return foo5(array, index/3) }

func main() {
    array := make([]int, 1000)
    fmt.Println(foo6(array, 50))
}

