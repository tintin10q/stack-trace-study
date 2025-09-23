// Build: v -g -o bin/v/main3 v/main3.v
fn dangerous(a []int, index int) int {
	return a[index]
}

fn foo(a []int, counter int) int {
	if counter == 0 {
		return dangerous(a, counter + 9137)
	}
	return foo(a, counter - 1)
}

fn main() {
	a := []int{len: 1000, init: 0}
	r := foo(a, 6)
	println('The result is ${r}')
}
