// dotnet fsi main3.fsx
let dangerous (arr:int[]) (index:int) : int = arr.[index] // IndexOutOfRangeException
let rec foo (arr:int[]) (counter:int) : int =
    if counter = 0 then dangerous arr (counter + 9137)
    else foo arr (counter - 1)

[<EntryPoint>]
let main _ =
    let arr = Array.zeroCreate<int> 1000
    let r = foo arr 900
    printfn "The result is %d" r
    0
