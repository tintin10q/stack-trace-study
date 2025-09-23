function dangerous(array::Vector{Int}, index::Int)::Int
    return array[index]  # BoundsError
end

function foo(array::Vector{Int}, counter::Int)::Int
    if counter == 0
        return dangerous(array, counter + 9137)
    else
        return foo(array, counter - 1)
    end
end

a = fill(0, 1000)
r = foo(a, 900)
println("The result is ", r)
