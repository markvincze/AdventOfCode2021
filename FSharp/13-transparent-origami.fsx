open System
open System.IO

// Part 1
let lines = File.ReadAllLines "13-transparent-origami-input.txt"
let coordLines, foldLines =
    lines
    |> Array.splitAt (Array.findIndex (fun (l : string) -> l.Length = 0) lines)

let coords = coordLines
             |> Array.map ((fun l -> l.Split ',') >>
                          (fun [|x;y|] -> (Int32.Parse x, Int32.Parse y)))
             |> List.ofArray

let folds = foldLines
            |> Array.skip 1
            |> Array.map
                   ((fun l -> l.Replace("fold along ", "")) >>
                   (fun l -> l.Split '=') >>
                   (fun [| axis; coord |] -> (axis, Int32.Parse coord)))
            |> List.ofArray

let paper = Array2D.init
                      (coords |> List.maxBy fst |> fst)
                      (coords |> List.maxBy snd |> snd)
                      (fun x y -> List.exists (fun (cx, cy) -> cx = x && cy = y) coords) 

let verticalFold foldY paper =
    let width = Array2D.length1 paper
    let height = Array2D.length2 paper

    let newHeight = foldY <= (height / 2)
                    then height - (foldY + 1)
                    else foldY - 1

    Array2D.init
        width
        newHeight
        (fun x y -> if foldY <= (height / 2)
                    then if cy < (height - foldY - 1 - foldY)
                         then paper[cx, height - 1 - cy]
                         else paper[cx, height - 1 - cy] || paper[cx, height - foldY - foldY ]
5 -> 0
6 -> 1

      X
      X
      X
      X
      X
#    #X
#    #X
- <- -
X
X
X
X
X
X
X