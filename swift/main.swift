func dangerous(_ array: [Int], _ index: Int) -> Int { return array[index + 2] }
func foo(_ array: [Int], _ index: Int) -> Int { return dangerous(array, index) }
func foo1(_ array: [Int], _ index: Int) -> Int { return foo(array, index * 3) }
func foo2(_ array: [Int], _ index: Int) -> Int { return foo1(array, index + 137) }
func foo3(_ array: [Int], _ index: Int) -> Int { return foo2(array, index - 1) }
func foo4(_ array: [Int], _ index: Int) -> Int { return foo3(array, index * 137) }
func foo5(_ array: [Int], _ index: Int) -> Int { return foo4(array, index + 20) }
func foo6(_ array: [Int], _ index: Int) -> Int { return foo5(array, index / 3) }
let array = Array(repeating: 0, count: 1000)
print(foo6(array, 50))

