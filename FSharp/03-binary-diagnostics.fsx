open System
open System.IO

let numbers = File.ReadAllLines "03-binary-diagnostics-input.txt" 
              |> Array.map (fun line -> line |> Seq.map (fun c -> c = '1') |> Array.ofSeq)
              |> List.ofArray

let toInt binary = binary
                   |> Array.rev
                   |> Array.indexed
                   |> Array.sumBy (fun (i, d) -> if d then (Math.Pow(2.0, i |> float) |> int) else 0)

let gammaRate numbers =
    let rec gammaRate (numbers : bool[] list) counters =
        match numbers with
        | [] -> counters |> Array.map (fun d -> d >= 0)
        | h :: t -> gammaRate
                        t 
                        (counters |> Array.mapi (fun i d -> if h.[i] then d + 1 else d - 1))
    
    gammaRate numbers (Array.create (numbers.Head.Length) 0)

let gammaRateBinary = gammaRate numbers
let epsilonRateBinary = gammaRateBinary |> Array.map (not)

let result1 = (toInt gammaRateBinary) * (toInt epsilonRateBinary)

// Part 2
let filter numbers (bitCriteriaGenerator : list<bool[]> -> bool[]) =
    let rec filter (numbers : bool[] list) i =
        let mask = bitCriteriaGenerator numbers
        let filteredNumbers = numbers |> List.filter (fun n -> n.[i] = mask.[i])
        if List.length filteredNumbers = 1
        then List.head filteredNumbers 
        else filter filteredNumbers (i + 1)
    
    filter numbers 0

let oxygenRatingBinary = filter numbers gammaRate
let co2ScrubberRating = filter numbers (gammaRate >> (Array.map not))

let result2 = (toInt oxygenRatingBinary) * (toInt co2ScrubberRating)