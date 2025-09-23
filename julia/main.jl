dangerous(array, index) = array[index + 2 + 1]  # Julia is 1-based
foo(array, index) = dangerous(array, index)
foo1(array, index) = foo(array, index * 3)
foo2(array, index) = foo1(array, index + 137)
foo3(array, index) = foo2(array, index - 1)
foo4(array, index) = foo3(array, index * 137)
foo5(array, index) = foo4(array, index + 20)
foo6(array, index) = foo5(array, fld(index, 3))  # trunc toward -∞; close enough for positive 50
function main()
    array = fill(0, 1000)
    println(foo6(array, 50))
end
main()

