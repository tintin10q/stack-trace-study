let dangerous v1 v2 = v1 / v2
let rec foo array counter =
  if counter = 0 then dangerous array.(0) counter else foo array (counter - 1)
let () =
  let array = Array.make 1000 0 in
  Printf.printf "The result is %d\n" (foo array 6)

