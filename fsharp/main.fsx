let dangerous (array:int[]) (index:int) : int = array.[index + 2]
let foo  a i = dangerous a i
let foo1 a i = foo a (i * 3)
let foo2 a i = foo1 a (i + 137)
let foo3 a i = foo2 a (i - 1)
let foo4 a i = foo3 a (i * 137)
let foo5 a i = foo4 a (i + 20)
let foo6 a i = foo5 a (i / 3)
let array = Array.create 1000 0
printfn "%d" (foo6 array 50)

