func dangerous(_ array: [Int], _ index: Int) -> Int {
    return array[index] // traps on OOB
}

func foo(_ array: [Int], _ counter: Int) -> Int {
    if counter == 0 { return dangerous(array, counter + 9137) }
    return foo(array, counter - 1)
}

let array = Array(repeating: 0, count: 1000)
let r = foo(array, 900)
print("The result is \(r)")
