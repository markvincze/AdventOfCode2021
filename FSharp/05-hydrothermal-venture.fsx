open System
open System.IO

type Line = (int * int) * (int * int)

let parseLine (line : string) =
    let [|p1; p2|] = line.Split " -> "

    let p1Split = p1.Split ','
    let p2Split = p2.Split ','
    ((p1Split.[0] |> Int32.Parse, p1Split.[1] |> Int32.Parse), (p2Split.[0] |> Int32.Parse, p2Split.[1] |> Int32.Parse))

let lines = File.ReadAllLines "05-hydrothermal-venture-input.txt"
            |> Array.map parseLine
            |> List.ofArray

let markPoint coverage (x, y) =
    match Map.tryFind (x, y) coverage with
    | None -> Map.add (x, y) 1 coverage
    | Some v -> Map.add (x, y) (v + 1) coverage

let range end1 end2 =
    if end2 > end1
    then [end1..end2]
    else [end2..end1] |> List.rev

let markCoveredPoints coverage ((startX, startY), (endX, endY)) =
    if startX = endX || startY = endY
    then seq {
             for x in range startX endX  do
                 for y in range startY endY do
                     yield (x, y) }
         |> Seq.fold markPoint coverage
    else List.zip (range startX endX) (range startY endY)
         |> Seq.fold markPoint coverage

let finalCoverage1 = lines
                     |> List.filter (fun ((startX, startY), (endX, endY)) -> startX = endX || startY = endY)
                     |> List.fold markCoveredPoints Map.empty<(int * int), int>

let result1 = finalCoverage1
              |> Map.filter (fun _ value -> value >= 2)
              |> Seq.length

let finalCoverage2 = lines
                     |> List.fold markCoveredPoints Map.empty<(int * int), int>

let result2 = finalCoverage2
              |> Map.filter (fun _ value -> value >= 2)
              |> Seq.length
