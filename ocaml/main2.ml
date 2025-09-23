let dangerous (a:int array) (index:int) : int =
  a.(index)  (* Invalid_argument "index out of bounds" *)

let rec foo (a:int array) (counter:int) : int =
  if counter = 0 then dangerous a (counter + 9137)
  else foo a (counter - 1)

let () =
  let a = Array.make 1000 0 in
  let r = foo a 6 in
  Printf.printf "The result is %d\n" r
