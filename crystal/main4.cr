def dangerous(v1 : Int32, v2 : Int32) : Int32
  v1 // v2
end
def foo(array : Array(Int32), counter : Int32) : Int32
  counter == 0 ? dangerous(array[0], counter) : foo(array, counter - 1)
end
array = Array(Int32).new(1000, 0)
puts "The result is #{foo(array, 6)}"

