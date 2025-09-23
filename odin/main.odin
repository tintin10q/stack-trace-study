package main

import "core:fmt"

dangerous :: proc(arr: []int, index: int) -> int {
    // intentionally allow out-of-bounds access to trigger a runtime panic
    return arr[index + 2]
}

foo  :: proc(arr: []int, index: int) -> int { return dangerous(arr, index) }
foo1 :: proc(arr: []int, index: int) -> int { return foo(arr, index * 3) }
foo2 :: proc(arr: []int, index: int) -> int { return foo1(arr, index + 137) }
foo3 :: proc(arr: []int, index: int) -> int { return foo2(arr, index - 1) }
foo4 :: proc(arr: []int, index: int) -> int { return foo3(arr, index * 137) }
foo5 :: proc(arr: []int, index: int) -> int { return foo4(arr, index + 20) }
foo6 :: proc(arr: []int, index: int) -> int { return foo5(arr, index / 3) }

main :: proc() {
    arr: [1000]int
    res := foo6(arr[:], 50)
    fmt.printf("The result is %d\n", res)
}
