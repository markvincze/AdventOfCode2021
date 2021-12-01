open System
open System.IO

let readings = File.ReadAllLines "01-sonar-sweep-input.txt"
               |> Array.map Int32.Parse
               |> List.ofArray

let increases readings =
    let rec increases readings acc previous =
        match readings with
        | [] -> acc
        | h :: t -> if h > previous
                    then increases t (acc + 1) h
                    else increases t acc h
    
    increases readings 0 Int32.MaxValue

let result1 = increases readings

let increases2 readings =
    let rec increases2 readings acc previous =
        match readings with
        | h1 :: h2 :: h3 :: t -> if h1 + h2 + h3 > previous
                                 then increases2 (h2 :: h3 :: t) (acc + 1) (h1 + h2 + h3)
                                 else increases2 (h2 :: h3 :: t) acc (h1 + h2 + h3)
        | _ -> acc

    increases2 readings 0 Int32.MaxValue

let result2 = increases2 readings
