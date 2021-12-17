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
                      ((coords |> List.maxBy fst |> fst) + 1)
                      ((coords |> List.maxBy snd |> snd) + 1)
                      (fun x y -> List.exists (fun (cx, cy) -> cx = x && cy = y) coords) 

let verticalFold foldY paper =
    let width = Array2D.length1 paper
    let height = Array2D.length2 paper

    let newHeight = if (foldY < (height / 2)) || ((height % 2 <> 0) && foldY <= (height / 2))
                    then max (height - (foldY + 1)) foldY
                    else foldY

    Array2D.init
        width
        newHeight
        (fun x y -> if (foldY < (height / 2)) || ((height % 2 <> 0) && foldY <= (height / 2))
                    then if y < (height - foldY - foldY - 1)
                         then paper.[x, height - 1 - y]
                         else paper.[x, height - 1 - y] || paper.[x, y - (height - foldY - foldY - 1) ]
                    else if y < (foldY - (height - foldY - 1))
                         then paper.[x, y]
                         else paper.[x, y] || paper.[x, foldY + (foldY - y)])

let horizontalFold foldX paper =
    let width = Array2D.length1 paper
    let height = Array2D.length2 paper

    let newWidth = if (foldX < (width / 2)) || ((width % 2 <> 0) && foldX <= (width / 2))
                   then max (width - (foldX + 1)) foldX
                   else foldX

    Array2D.init
        newWidth
        height
        (fun x y -> if (foldX < (width / 2)) || ((width % 2 <> 0) && foldX <= (width / 2))
                    then if x < (width - foldX - foldX - 1)
                         then paper.[width - 1 - x, y]
                         else paper.[width - 1 - x, y] || paper.[x - (width - foldX - foldX - 1), y]
                    else if y < (foldX - (width - foldX - 1))
                         then paper.[x, y]
                         else paper.[x, y] || paper.[foldX + (foldX - x), y])

let score paper = 
    let width = Array2D.length1 paper
    let height = Array2D.length2 paper

    seq { for y in 0..height-1 do
              for x in 0..width-1 do
                  yield x, y }
    |> Seq.filter (fun (x, y) -> paper.[x, y])
    |> Seq.length

let print paper =
    let width = Array2D.length1 paper
    let height = Array2D.length2 paper

    for y in 0..height-1 do
        for x in 0..width-1 do
            printf "%c" (if paper.[x, y] then '#' else '.')
        printfn ""

let applyFold fold paper =
    match fold with
    | ("x", foldX) -> horizontalFold foldX paper
    | ("y", foldY) -> verticalFold foldY paper

let firstFold = folds |> List.head
let afterFirstFold = applyFold firstFold paper

let result1 = score afterFirstFold

let afterAllFolds =
    folds
    |> List.fold (fun p fold -> applyFold fold p) paper
