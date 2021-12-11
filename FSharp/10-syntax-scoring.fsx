open System
open System.IO

type CheckResult =
| Legal
| Incomplete
| Corrupted of char

let opener = function
             | ')' -> '('
             | ']' -> '['
             | '}' -> '{'
             | '>' -> '<'

let check line =
    let rec check line stack =
        match line with
        | [] -> if List.isEmpty stack
                then Legal, stack
                else Incomplete, stack
        | h :: t -> match h with
                    | '(' | '[' | '{' | '<' -> check t (h :: stack)
                    | ')' | ']' | '}' | '>' ->
                        match stack with
                        | hs :: ts -> if hs = opener h
                                      then check t ts
                                      else Corrupted h, stack
                        | [] -> Corrupted h, stack
    check line []

let lines = File.ReadAllLines "10-syntax-scoring-input.txt"
            |> Array.map List.ofSeq
            |> List.ofArray

let score = function
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | '>' -> 25137

let result1 = lines
              |> List.map check
              |> List.sumBy (fun r -> match fst r with
                                      | Corrupted c -> score c
                                      | _ -> 0)

let autocompleteScore chars =
    let rec autocompleteScore chars acc =
        match chars with
        | [] -> acc
        | h :: t -> let cs = match h with
                             | '(' -> 1L
                             | '[' -> 2L
                             | '{' -> 3L
                             | '<' -> 4L
                    autocompleteScore t (acc * 5L + cs)
    autocompleteScore chars 0L

let incompletes = lines
                  |> List.map check
                  |> List.choose (fun r -> match r with
                                           | Incomplete, stack -> Some stack
                                           | _ -> None)

let scores = incompletes |> List.map autocompleteScore

let result2 = incompletes
              |> List.map autocompleteScore
              |> List.sort
              |> List.item ((List.length incompletes) / 2)
