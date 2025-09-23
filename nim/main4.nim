proc dangerous(v1:int, v2:int): int = v1 div v2
proc foo(array: seq[int], counter:int): int =
  if counter == 0: dangerous(array[0], counter)
  else: foo(array, counter - 1)
when isMainModule:
  var array = newSeq[int](1000)
  echo "The result is ", foo(array, 6)

