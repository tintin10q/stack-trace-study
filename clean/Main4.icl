module Main4
import StdEnv

dangerous :: Int Int -> Int
dangerous v1 v2 = v1 / v2   // integer division; will crash on v2 == 0

foo :: {#Int} Int -> Int
foo array counter
| counter == 0 = dangerous array.[0] counter
= foo array (counter - 1)

Start :: Int
Start =
    let array = createArray 1000 0
    in foo array 6
