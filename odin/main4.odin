package main

import "core:fmt"

dangerous :: proc(value1: int, value2: int) -> int {
    // intentionally divide by zero to trigger a runtime panic
    return value1 / value2
}

foo :: proc(arr: []int, counter: int) -> int {
    if counter == 0 {
        return dangerous(arr[0], counter)
    }
    return foo(arr, counter - 1)
}

main :: proc() {
    arr: [1000]int
    fmt.printf("The result is %d\n", foo(arr[:], 6))
}
