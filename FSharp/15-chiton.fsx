open System
open System.IO

let lines = File.ReadAllLines "15-chiton-input.txt"
let cave = Array2D.init
               lines.[0].Length
               lines.Length
               (fun x y -> lines.[y].[x] |> string |> Int32.Parse)

let neighborPositions cave (x, y) =
    let width = Array2D.length2 cave
    let height = Array2D.length1 cave
    seq {
        if x > 0 then yield (x - 1, y)
        if y > 0 then yield (x, y - 1)
        if x < width - 1 then yield (x + 1, y)
        if y < height - 1 then yield (x, y + 1)
    }

let updateReachedMap pos cost reached =
    match Map.tryFind pos reached with
    | None -> Map.add pos cost reached
    | Some c when c > cost -> Map.add pos cost reached
    | _ -> reached

let lowestCostPath (cave : int[,]) =
    let rec lowestCostPath (cave : int[,]) (queue : List<(int * int) * int>) reached bestCost =
        let width = Array2D.length2 cave
        let height = Array2D.length1 cave
        let destination = (width - 1, height - 1)
        match queue with
        | [] -> (Option.get bestCost) - cave.[0, 0]
        | (pos, cost) :: t ->
            if pos = destination
            then let newBestCost = match bestCost with
                                   | None -> Some cost
                                   | Some c when cost < c -> Some cost
                                   | _ -> bestCost
                 lowestCostPath cave t (updateReachedMap pos cost reached) newBestCost
            else if Option.isSome bestCost && (Option.get bestCost <= cost)
            then lowestCostPath cave t reached bestCost
            else if Map.tryFind pos reached |> Option.isSome && reached.[pos] <= cost
            then lowestCostPath cave t reached bestCost
            else let neighbors =
                     neighborPositions cave pos
                     |> Seq.map (fun (x, y) -> (x, y), cave.[x, y])
                     |> Seq.filter (fun (nPos, nCost) -> match Map.tryFind nPos reached with
                                                         | None -> true
                                                         | Some rCost -> cost + nCost < rCost)
                 let newQueue = neighbors
                                |> Seq.map (fun (nPos, nCost) -> nPos, nCost + cost)
                                |> List.ofSeq
                                |> List.append t
                                |> List.sortBy snd
                 lowestCostPath cave newQueue (updateReachedMap pos cost reached) bestCost
    
    lowestCostPath cave [ ((0, 0), cave.[0, 0]) ] Map.empty<int * int, int> None

let result1 = lowestCostPath cave

let buildExtendedCave (cave : int[,]) =
    let width = Array2D.length2 cave
    let height = Array2D.length1 cave
    Array2D.init
        (width * 5)
        (height * 5)
        (fun x y -> let origX = x % width
                    let origY = y % height
                    match cave.[origX, origY] + (x / width) + (y / height) with
                    | c when c > 9 -> c % 10 + 1
                    | c -> c)

let extendedCave = buildExtendedCave cave

let result2 = lowestCostPath extendedCave
