fn dangerous(array []int, index int) int { return array[index + 2] }
fn foo(array []int, index int) int { return dangerous(array, index) }
fn foo1(array []int, index int) int { return foo(array, index * 3) }
fn foo2(array []int, index int) int { return foo1(array, index + 137) }
fn foo3(array []int, index int) int { return foo2(array, index - 1) }
fn foo4(array []int, index int) int { return foo3(array, index * 137) }
fn foo5(array []int, index int) int { return foo4(array, index + 20) }
fn foo6(array []int, index int) int { return foo5(array, index / 3) }
fn main(){
    array := []int{len: 1000, init: 0}
    println(foo6(array, 50))
}

