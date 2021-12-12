open System
open System.IO

let lines = File.ReadAllLines "09-smoke-basin-input.txt"
let cave = Array2D.init
               (Array.length lines)
               (lines.[0].Length)
               (fun y x -> string (lines.[y].[x]) |> Int32.Parse)

let width = Array2D.length2 cave
let height = Array2D.length1 cave

let neighborPositions (x, y) =
    seq {
        if x > 0 then yield (x - 1, y)
        if y > 0 then yield (x, y - 1)
        if x < width - 1 then yield (x + 1, y)
        if y < height - 1 then yield (x, y + 1)
    }

let neighbors (x, y) (cave : int[,]) =
    neighborPositions (x, y) |> Seq.map (fun (x, y) -> cave.[y, x])

let isLowPoint x y cave =
    neighbors (x, y) cave
    |> Seq.forall (fun n -> n > cave.[y, x])

let lowPoints = 
    seq {
        for y in 0..height-1 do
            for x in 0..width-1 do
                yield x, y }
    |> Seq.filter (fun (x, y) -> isLowPoint x y cave)
    |> List.ofSeq

let result1 = lowPoints
              |> List.sumBy (fun (x, y) -> cave.[y, x] + 1)

// Part 2
let basinSize cave lowPoint  =
    let rec basinSize (cave : int[,]) visited queue acc =
        match queue with
        | [] -> acc
        | (x, y) :: t ->
            if List.contains (x, y) visited
            then basinSize cave visited t acc
            else let newQueue =
                     neighborPositions (x, y)
                     |> Seq.filter (fun p -> not (List.contains p visited))
                     |> Seq.filter (fun (nx, ny) -> cave.[ny, nx] <> 9 && cave.[ny, nx] >= cave.[y, x])
                     |> List.ofSeq
                     |> List.append t
                 basinSize cave ((x, y) :: visited) newQueue (acc + 1)
    
    basinSize cave [] [ lowPoint ] 0

let [b1; b2; b3] = lowPoints
                   |> List.map (basinSize cave)
                   |> List.sortDescending
                   |> List.take 3

let result2 = b1 * b2 * b3
