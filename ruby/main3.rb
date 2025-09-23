def dangerous(array, index)
  array[index] + 1  # MODIFIED TO make error nil+1 -> NoMethodError
end

def foo(array, counter)
  if counter == 0
    return dangerous(array, counter + 9137)
  end
  foo(array, counter - 1)
end

array = Array.new(1000, 0)
result = foo(array, 900)
puts "The result is #{result}"
