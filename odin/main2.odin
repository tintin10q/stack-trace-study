package main

import "core:fmt"

dangerous :: proc(a: []int, index: int) -> int {
    return a[index] // panic on OOB in debug
}

foo :: proc(a: []int, counter: int) -> int {
    if counter == 0 {
        return dangerous(a, counter + 9137)
    }
    return foo(a, counter - 1)
}

main :: proc() {
    a := make([]int, 1000)
    r := foo(a, 6)
    fmt.println("The result is", r)
}
