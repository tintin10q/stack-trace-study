module Main3
import StdEnv

dangerous :: {#Int} Int -> Int
dangerous a i = a.[i] // should cause index error

foo :: {#Int} Int -> Int
foo a c
| c == 0    = dangerous a (c + 9137)
= foo a (c - 1)

Start :: Int
Start
# a = createArray 1000 0
# r = foo a 900
= r
