dangerous :: Int -> Int -> Int
dangerous v1 v2 = v1 `div` v2
foo :: [Int] -> Int -> Int
foo array counter = if counter == 0 then dangerous (array !! 0) counter else foo array (counter - 1)
main :: IO ()
main = do
  let array = replicate 1000 0
  print (foo array 6)

