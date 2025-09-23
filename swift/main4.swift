func dangerous(_ v1:Int, _ v2:Int) -> Int { v1 / v2 }
func foo(_ array:[Int], _ counter:Int) -> Int {
  if counter == 0 { return dangerous(array[0], counter) }
  return foo(array, counter-1)
}
let array = Array(repeating: 0, count: 1000)
print("The result is \(foo(array, 6))")

