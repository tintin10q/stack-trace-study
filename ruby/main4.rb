def dangerous(v1, v2) v1 / v2 end
def foo(array, counter)
  return dangerous(array[0], counter) if counter == 0
  foo(array, counter - 1)
end
array = Array.new(1000, 0)
puts "The result is #{foo(array, 6)}"

