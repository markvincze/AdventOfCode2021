open System
open System.IO

// Part 1
let parse (line : string) = let sections = line.Split " | "
                            (sections.[0].Split ' ', sections.[1].Split ' ')

let digits = File.ReadAllLines "08-seven-segment-search-input.txt"
             |> Array.map parse
             |> Array.map snd

let result1 = digits
              |> Array.concat
              |> Array.filter (fun d -> d.Length = 2 || d.Length = 4 || d.Length = 3 || d.Length = 7)
              |> Array.length

// Part 2
let findMappings (patterns : string[]) =
    let pattern1 = patterns |> Array.find (fun p -> p.Length = 2)
    let pattern3 = patterns
                   |> Array.filter (fun p -> p.Length = 5)
                   |> Array.find (fun p -> p |> Seq.except pattern1 |> Seq.length = 3)
    let pattern4 = patterns |> Array.find (fun p -> p.Length = 4)
    let pattern7 = patterns |> Array.find (fun p -> p.Length = 3)
    let pattern8 = patterns |> Array.find (fun p -> p.Length = 7)
    let pattern9 = patterns
                   |> Array.filter (fun p -> p.Length = 6)
                   |> Array.find (fun p -> p |> Seq.except pattern4 |> Seq.length = 2)

    let e = pattern8 |> Seq.except pattern9 |> Seq.head

    let pattern2 = patterns
                   |> Array.filter (fun p -> p.Length = 5)
                   |> Array.filter (fun p -> p |> Seq.except pattern3 |> Seq.length = 1)
                   |> Array.find (fun p -> p |> Seq.except pattern3 |> Seq.head = e)
    
    let pattern5 = patterns
                   |> Array.filter (fun p -> p.Length = 5)
                   |> Array.except [| pattern3; pattern2 |]
                   |> Array.head
    
    let pattern6 = patterns
                   |> Array.filter (fun p -> p.Length = 6)
                   |> Array.find (fun p -> p |> Seq.except pattern1 |> Seq.length = 5)
    
    let pattern0 = patterns
                   |> Array.filter (fun p -> p.Length = 6)
                   |> Array.except [| pattern9; pattern6 |]
                   |> Array.head

    [ pattern0; pattern1; pattern2; pattern3; pattern4; pattern5; pattern6; pattern7; pattern8; pattern9 ]
    |> List.map Set.ofSeq
    |> List.mapi (fun i p -> (p, i))
    |> Map.ofList

let patternsMatch p1 p2 = Set.ofSeq p1 = Set.ofSeq p2

let output patterns (digits : string[]) =
    let patternMapping = findMappings patterns
    let d0 = patternMapping.[digits.[0] |> Set.ofSeq]
    let d1 = patternMapping.[digits.[1] |> Set.ofSeq]
    let d2 = patternMapping.[digits.[2] |> Set.ofSeq]
    let d3 = patternMapping.[digits.[3] |> Set.ofSeq]
    d0 * 1000 + d1 * 100 + d2 * 10 + d3

let result2 = File.ReadAllLines "08-seven-segment-search-input.txt"
              |> Array.map parse
              |> Array.map (fun (patterns, digits) -> output patterns digits)
              |> Array.sum
