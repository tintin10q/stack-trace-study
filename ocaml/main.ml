let dangerous array index = array.(index + 2)
let foo array index = dangerous array index
let foo1 array index = foo array (index * 3)
let foo2 array index = foo1 array (index + 137)
let foo3 array index = foo2 array (index - 1)
let foo4 array index = foo3 array (index * 137)
let foo5 array index = foo4 array (index + 20)
let foo6 array index = foo5 array (index / 3)
let () =
  let array = Array.make 1000 0 in
  Printf.printf "%d\n" (foo6 array 50)

