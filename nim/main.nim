proc dangerous(array: seq[int], index: int): int = array[index + 2]
proc foo(array: seq[int], index: int): int = dangerous(array, index)
proc foo1(array: seq[int], index: int): int = foo(array, index * 3)
proc foo2(array: seq[int], index: int): int = foo1(array, index + 137)
proc foo3(array: seq[int], index: int): int = foo2(array, index - 1)
proc foo4(array: seq[int], index: int): int = foo3(array, index * 137)
proc foo5(array: seq[int], index: int): int = foo4(array, index + 20)
proc foo6(array: seq[int], index: int): int = foo5(array, index div 3)
when isMainModule:
  var array = newSeq[int](1000)
  echo foo6(array, 50)

