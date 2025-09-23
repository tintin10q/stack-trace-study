let dangerous (v1:int) (v2:int) : int = v1 / v2
let rec foo (array:int[]) (counter:int) : int =
  if counter = 0 then dangerous array.[0] counter else foo array (counter - 1)
let array = Array.create 1000 0
printfn "The result is %d" (foo array 6)

