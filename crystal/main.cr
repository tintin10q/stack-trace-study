def dangerous(array : Array(Int32), index : Int32) : Int32
  array[index + 2]
end
def foo(array : Array(Int32), index : Int32) : Int32
  dangerous(array, index)
end
def foo1(array : Array(Int32), index : Int32) : Int32
  foo(array, index * 3)
end
def foo2(array : Array(Int32), index : Int32) : Int32
  foo1(array, index + 137)
end
def foo3(array : Array(Int32), index : Int32) : Int32
  foo2(array, index - 1)
end
def foo4(array : Array(Int32), index : Int32) : Int32
  foo3(array, index * 137)
end
def foo5(array : Array(Int32), index : Int32) : Int32
  foo4(array, index + 20)
end
def foo6(array : Array(Int32), index : Int32) : Int32
  foo5(array, index // 3)
end
array = Array(Int32).new(1000, 0)
puts foo6(array, 50)

