open System
open System.IO

let parse lines =
    let rec parse (lines : string list) caves =
        match lines with
        | [] -> caves
        | h :: t -> let [|from; toward|] = h.Split '-'
                    let caves = match Map.tryFind from caves with
                                | None -> Map.add from [ toward ] caves
                                | Some ts -> Map.add from (toward :: ts) caves
                    let caves = match Map.tryFind toward caves with
                                | None -> Map.add toward [ from ] caves
                                | Some fs -> Map.add toward (from :: fs) caves
                    parse t caves 
    parse lines Map.empty<string, string list>

let caves = File.ReadAllLines "12-passage-pathing-input.txt" |> List.ofArray |> parse 

let isSmallCave (name : string) = name.[0] |> Char.IsLower
let isLargeCave (name : string) = name.[0] |> Char.IsUpper
    
let rec routeCount (caves : Map<string, string list>) from visited visitedTwice isPart2 =
    if from = "end"
    then 1
    else let neighbors = caves.[from]
         let allowedNeighbors =
            if isPart2
            then neighbors
                 |> List.filter (fun n -> isLargeCave n || (n <> "start" && not visitedTwice) || (not <| Set.contains n visited))
            else neighbors
                 |> List.filter (fun n -> isLargeCave n || (not <| Set.contains n visited))
         List.sumBy
            (fun n -> routeCount caves n (Set.add from visited) (visitedTwice || (isSmallCave n && Set.contains n visited)) isPart2)
            allowedNeighbors

let result1 = routeCount caves "start" Set.empty<string> false false
let result2 = routeCount caves "start" Set.empty<string> false true
