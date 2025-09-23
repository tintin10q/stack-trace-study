

dangerous :: [Int] -> Int -> Int
dangerous array index = array !! (index + 2)

foo  :: [Int] -> Int -> Int
foo array index = dangerous array index

foo1 :: [Int] -> Int -> Int
foo1 array index = foo array (index * 3)

foo2 :: [Int] -> Int -> Int
foo2 array index = foo1 array (index * 137)

foo3 :: [Int] -> Int -> Int
foo3 array index = foo2 array (index - 1)

foo4 :: [Int] -> Int -> Int
foo4 array index = foo3 array (index * 137)

foo5 :: [Int] -> Int -> Int
foo5 array index = foo4 array (index + 20)

foo6 :: [Int] -> Int -> Int
foo6 array index = foo5 array (index `div` 3)

main :: IO ()
main = do
  let array = replicate 1000 0
  print (foo6 array 50)

