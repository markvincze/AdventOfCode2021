open System
// target area: x=79..137, y=-176..-117
let xMin = 79
let xMax = 137
let yMin = -176
let yMax = -117

let velocityYMax = yMin * -1 - 1
let result1 = ((1 + velocityYMax) / 2) * velocityYMax

// Part2
let velocityXMin = Math.Sqrt(float xMin) |> int
let velocityXMax = xMax
let velocityYMin = yMin

let rec fallsWithin x y vx vy xMin xMax yMin yMax =
    if x >= xMin && x <= xMax && y >= yMin && y <= yMax
    then true
    else if x > xMax || y < yMin
    then false
    else let newVx = if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else 0
         fallsWithin (x + vx) (y + vy) (newVx) (vy - 1) xMin xMax yMin yMax

let result2 =
    seq {
        for vx in velocityXMin..velocityXMax do
            for vy in velocityYMin..velocityYMax do
                yield vx, vy }
    |> Seq.filter (fun (vx, vy) -> fallsWithin 0 0 vx vy xMin xMax yMin yMax)
    |> Seq.length

