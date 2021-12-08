open System
open System.IO

let positions = (File.ReadAllText "07-treachery-of-whales-input.txt").Split ','
                |> Array.map Int32.Parse
                |> List.ofArray

let minPos = positions |> List.min
let maxPos = positions |> List.max

let moveCost1 (dist : int) = dist

let calcCost costFunction positions (targetPos : int) =
    positions |> List.sumBy (fun p -> costFunction (Math.Abs (targetPos - p)))

let result1 = [minPos..maxPos]
              |> List.map (calcCost moveCost1 positions)
              |> List.min

let moveCost2 (dist : int) = ((1.0 + (float dist))/2.0 * (float dist)) |> int

let result2 = [minPos..maxPos]
              |> List.map (calcCost moveCost2 positions)
              |> List.min
