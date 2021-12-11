open System
open System.IO

let lines = File.ReadAllLines "11-dumbo-octopus-input.txt"

let octos = Array2D.init
                (lines.[0].Length)
                (Array.length lines)
                (fun x y -> lines.[y].[x] |> string |> Int32.Parse)

let width = Array2D.length1 octos
let height = Array2D.length2 octos
let allCoordinates =
    seq {
        for x in 0..width - 1 do
            for y in 0..height - 1 do
                yield (x, y) }

let neighborPositions (x, y) =
    seq {
        if x > 0 then yield (x - 1, y)
        if x > 0 && y > 0 then yield (x - 1, y - 1)
        if x > 0 && y < height - 1 then yield (x - 1, y + 1)
        if x < width - 1 then yield (x + 1, y)
        if x < width - 1 && y > 0 then yield (x + 1, y - 1)
        if x < width - 1 && y < height - 1 then yield (x + 1, y + 1)
        if y > 0 then yield (x, y - 1)
        if y < height - 1 then yield (x, y + 1)
    }

let evolve (octos : int[,]) =
    for x in 0..width - 1 do
        for y in 0..height - 1 do
            octos.[x, y] <- octos.[x, y] + 1

    let oneFlash (octos : int[,]) (x, y) =
        for (nx, ny) in neighborPositions (x, y) do
            octos.[nx, ny] <- match octos.[nx, ny] with
                              | -1 -> -1
                              | value -> value + 1
            octos.[x, y] <- -1
        octos
    
    let rec flash (octos : int[,]) acc =
        let flashes = allCoordinates
                      |> Seq.filter (fun (x, y) -> octos.[x, y] > 9)
                      |> List.ofSeq
        match flashes with
        | [] -> octos, acc
        | flashes -> flash (List.fold oneFlash octos flashes) (acc + (List.length flashes))
    
    let octos, flashCount = flash octos 0

    allCoordinates
    |> Seq.iter (fun (x, y) -> if octos.[x, y] = -1
                               then octos.[x, y] <- 0)
    
    octos, flashCount
    
let evolveN n octos =
    let rec evolveN n octos acc =
        match n with
        | 0 -> acc
        | n -> let octos, flashCount = evolve octos
               evolveN (n - 1) octos (acc + flashCount)
    evolveN n octos 0

let result1 = evolveN 100 octos

let evolveUntilSync octos =
    let rec evolveUntilSync octos n =
        let octos, flashCount = evolve octos
        if flashCount = (width * height)
        then n + 1
        else evolveUntilSync octos (n + 1)
    evolveUntilSync octos 0

let octos2 = Array2D.init
                (lines.[0].Length)
                (Array.length lines)
                (fun x y -> lines.[y].[x] |> string |> Int32.Parse)

let result2 = evolveUntilSync octos2

