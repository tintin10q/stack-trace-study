def dangerous(array, index) array[index + 2] + 2  end # modified to make error
def foo(array, index) dangerous(array, index) end
def foo1(array, index) foo(array, index * 3) end
def foo2(array, index) foo1(array, index + 137) end
def foo3(array, index) foo2(array, index - 1) end
def foo4(array, index) foo3(array, index * 137) end
def foo5(array, index) foo4(array, index + 20) end
def foo6(array, index) foo5(array, index / 3) end
array = Array.new(1000, 0)
puts foo6(array, 50)

