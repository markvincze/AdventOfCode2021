open System
open System.IO

type Command =
| Forward of int
| Down of int
| Up of int

let parse (line : string) =
    let split = line.Split ' '
    match split.[0] with
    | "forward" -> Forward (split.[1] |> Int32.Parse)
    | "down" -> Down (split.[1] |> Int32.Parse)
    | "up" -> Up (split.[1] |> Int32.Parse)
    | _ -> failwith "Invalid input"

let commands = File.ReadAllLines "02-dive-input.txt"
               |> Array.map parse
               |> List.ofArray

let rec follow horizontalPos depth commands =
    match commands with
    | [] -> horizontalPos * depth
    | h :: t -> match h with
                | Forward x -> follow (horizontalPos + x) depth t
                | Down x -> follow horizontalPos (depth + x) t
                | Up x -> follow horizontalPos (depth - x) t

let result1 = follow 0 0 commands

let rec follow2 horizontalPos depth aim commands =
    match commands with
    | [] -> horizontalPos * depth
    | h :: t -> match h with
                | Forward x -> follow2 (horizontalPos + x) (depth + (aim * x)) aim t
                | Down x -> follow2 horizontalPos depth (aim + x) t
                | Up x -> follow2 horizontalPos depth (aim - x) t

let result2 = follow2 0 0 0 commands
