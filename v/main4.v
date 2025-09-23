fn dangerous(v1 int, v2 int) int { return v1 / v2 }
fn foo(array []int, counter int) int {
  if counter == 0 { return dangerous(array[0], counter) }
  return foo(array, counter-1)
}
fn main(){ array := []int{len:1000, init:0} println('The result is ${foo(array, 6)}') }

