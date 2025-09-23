def dangerous(array : Array(Int32), index : Int32) : Int32
  array[index] # IndexError
end

def foo(array : Array(Int32), counter : Int32) : Int32
  if counter == 0
    return dangerous(array, counter + 9137)
  end
  foo(array, counter - 1)
end

array = Array(Int32).new(1000, 0)
result = foo(array, 6)
puts "The result is #{result}"
