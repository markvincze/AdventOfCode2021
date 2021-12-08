open System
open System.IO

let timerCounts = Array.create 9 0L

let input = (File.ReadAllText "06-lanternfish-input.txt").Split ','
            |> Array.map Int32.Parse
            |> Array.iter (fun i -> timerCounts.[i] <- timerCounts.[i] + 1L)

let evolve (timerCounts : int64[]) =
    let newCounts = Array.create 9 0L
    for i in 0..7 do
        newCounts.[i] <- timerCounts.[i + 1]
    newCounts.[8] <- timerCounts.[0]
    newCounts.[6] <- newCounts.[6] + timerCounts.[0]
    newCounts

let rec evolveN n timerCounts =
    match n with
    | 0 -> timerCounts
    | n -> evolveN (n - 1) (evolve timerCounts)

let after80 = evolveN 80 timerCounts

let result1 = after80 |> Array.sum

let after256 = evolveN 256 timerCounts

let result2 = after256 |> Array.sum
