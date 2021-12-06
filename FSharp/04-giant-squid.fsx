open System
open System.IO

let parse (lines : string[]) =
    let drawings = lines.[0].Split ',' |> Array.map Int32.Parse |> List.ofArray

    let parseBoard (lines : string[]) =
        let numbers = lines |> Array.map (fun l -> l.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse)
        Array2D.init 5 5 (fun x y -> (numbers.[y].[x], false))

    let rec parse (lines : string[]) boards =
        let board = Array.take 5 lines |> parseBoard
        if lines.Length > 5
        then parse (Array.skip 6 lines) (board :: boards)
        else board :: boards

    drawings, (parse (Array.skip 2 lines) [])

let isWinningBoard board =
    let rowMarked y board = [0..4] |> List.forall (fun x -> (Array2D.get board y x) |> snd)
    let columnMarked x board = [0..4] |> List.forall (fun y -> (Array2D.get board y x) |> snd)

    [0..4] |> List.exists (fun y -> rowMarked y board) ||
    [0..4] |> List.exists (fun x -> columnMarked x board)

let score board lastNumber =
    let notMarkedNumsSum =
        seq {
            for y in 0..4 do
                for x in 0..4 do
                    yield (x, y) }
        |> Seq.sumBy (fun (x, y) -> match Array2D.get board y x with
                                    | (num, false) -> num
                                    | (_, true) -> 0)
    notMarkedNumsSum * lastNumber

let markNumber number (board : (int * bool)[,]) =
    for y in 0..4 do
        for x in 0..4 do
            if board.[y, x] |> fst = number
            then board.[y, x] <- (number, true)

let rec firstWinner boards drawings =
    match drawings with
    | [] -> failwith "Ran out of numbers before anybody won."
    | h :: t -> boards |> List.iter (fun b -> markNumber h b)
                match List.tryFind isWinningBoard boards with
                | Some winner -> score winner h
                | None -> firstWinner boards t

let drawings, boards = parse (File.ReadAllLines "04-giant-squid-input.txt")

let result1 = firstWinner boards drawings

let rec lastWinner boards drawings =
    match drawings with
    | [] -> failwith "Ran out of numbers before anybody won."
    | h :: t -> boards |> List.iter (fun b -> markNumber h b)
                if List.length boards = 1 && List.head boards |> isWinningBoard
                then score (List.head boards) h
                else lastWinner (boards |> List.filter (isWinningBoard >> not)) t

let result2 = lastWinner boards drawings
