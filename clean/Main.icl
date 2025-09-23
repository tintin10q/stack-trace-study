module Main
import StdEnv

dangerous :: {#Int} Int -> Int
dangerous a i = a.[i + 2]

foo  :: {#Int} Int -> Int
foo  a i = dangerous a i

foo1 :: {#Int} Int -> Int
foo1 a i = foo  a (i * 3)

foo2 :: {#Int} Int -> Int
foo2 a i = foo1 a (i + 137)

foo3 :: {#Int} Int -> Int
foo3 a i = foo2 a (i - 1)

foo4 :: {#Int} Int -> Int
foo4 a i = foo3 a (i * 137)

foo5 :: {#Int} Int -> Int
foo5 a i = foo4 a (i + 20)

foo6 :: {#Int} Int -> Int
foo6 a i = foo5 a (i / 3)   

Start :: Int
Start = let a = createArray 1000 0 in (foo6 a 50)
