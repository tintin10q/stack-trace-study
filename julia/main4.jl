dangerous(v1::Int, v2::Int) = div(v1, v2)
function foo(array::Vector{Int}, counter::Int)
    if counter == 0
        return dangerous(array[1], counter)
    else
        return foo(array, counter - 1)
    end
end
array = fill(0, 1000)
println("The result is ", foo(array, 6))

