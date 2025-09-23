-- Build with: ghc -rtsopts -O0 -g -o bin/haskell/main3 Main3.hs
-- Run with: ./bin/haskell/main3 +RTS -xc
module Main where

dangerous :: [Int] -> Int -> Int
dangerous arr idx = arr !! idx  -- will error

foo :: [Int] -> Int -> Int
foo arr counter
  | counter == 0 = dangerous arr (counter + 9137)
  | otherwise    = foo arr (counter - 1)

main :: IO ()
main = do
  let arr = replicate 1000 0
  let r = foo arr 6
  putStrLn ("The result is " ++ show r)
