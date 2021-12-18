open System
open System.Text
open System.IO

let lines = File.ReadAllLines "14-extended-polymerization-input.txt"
let template = lines.[0]
let rules = lines
            |> Array.skip 2
            |> Array.map (fun l -> let [| f; t |] = l.Split " -> "
                                   (f, t.[0]))
            |> List.ofArray

let apply rules template =
    let applyRule ((between : string), element) (template : string) =
        seq { for i in 0..template.Length - 2 do
                  if template.[i] = between.[0] && template.[i+1] = between.[1]
                  then yield (i + 1, element)}
    
    let inserts = rules
                  |> Seq.collect (fun r -> applyRule r template)
                  |> List.ofSeq
                  |> List.sortBy fst
    // inserts

    let rec collectWithInserts (sb : StringBuilder) (template : string) index newLength (inserts : (int * char) list) insertsProcessed =
        if index > newLength - 1
        then sb.ToString()
        else match inserts with
             | [] -> sb.Append (template.[index - insertsProcessed]) |> ignore
                     collectWithInserts sb template (index + 1) newLength inserts insertsProcessed
             | (ii, c) :: t -> if index = ii + insertsProcessed
                               then sb.Append c |> ignore
                                    collectWithInserts sb template (index + 1) newLength t (insertsProcessed + 1)
                               else sb.Append (template.[index - insertsProcessed]) |> ignore
                                    collectWithInserts sb template (index + 1) newLength inserts insertsProcessed

    collectWithInserts (StringBuilder()) template 0 (template.Length + (List.length inserts)) inserts 0

// let after1 = apply rules template
// let after2 = apply rules after1
// let after3 = apply rules after2
// let after4 = apply rules after3

let rec applyN rules template n =
    printfn "Applying rules, %d times left" n
    if n = 0
    then template
    else applyN rules (apply rules template) (n - 1)

let collectCounts polymer =
    let rec collectCounts (polymer : string) counts index =
        if index >= polymer.Length
        then counts
        else let newCounts = match Map.tryFind (polymer.[index]) counts with
                             | None -> Map.add (polymer.[index]) 1L counts
                             | Some n -> Map.add (polymer.[index]) (n + 1L) counts
             collectCounts polymer newCounts (index + 1)
    collectCounts polymer Map.empty<char, int64> 0

let after10 = applyN rules template 10
let counts1 = collectCounts after10
let result1 = (counts1 |> Seq.map (fun kvp -> kvp.Value) |> Seq.max) -
              (counts1 |> Seq.map (fun kvp -> kvp.Value) |> Seq.min)

// let after40 = applyN rules template 40
// let counts2 = collectCounts after10
// let result2 = (counts2 |> Seq.map (fun kvp -> kvp.Value) |> Seq.max) -
//               (counts2 |> Seq.map (fun kvp -> kvp.Value) |> Seq.min)

