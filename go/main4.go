package main
import "fmt"
func dangerous(value1 int, value2 int) int { return value1 / value2 }
func foo(array []int, counter int) int {
    if counter == 0 { return dangerous(array[0], counter) }
    return foo(array, counter-1)
}
func main(){ array := make([]int, 1000); fmt.Printf("The result is %d\n", foo(array, 6)) }

