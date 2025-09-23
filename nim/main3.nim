proc dangerous(a: seq[int], index: int): int =
  result = a[index] # IndexError

proc foo(a: seq[int], counter: int): int =
  if counter == 0:
    result = dangerous(a, counter + 9137)
  else:
    result = foo(a, counter - 1)

when isMainModule:
  var a = newSeq[int](1000)
  let r = foo(a, 900)
  echo "The result is ", r
