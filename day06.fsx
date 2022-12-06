# time

let input =
    "inputs/day06.txt"
    |> System.IO.File.ReadAllText

let findFirstUniqueIndex length text =
    text
    |> Seq.toArray
    |> Array.windowed length
    |> Array.map set
    |> Array.findIndex (fun x -> x.Count = length)
    |> fun x -> x + length

input
    |> findFirstUniqueIndex 4
    |> printfn "Part one %A"

input
    |> findFirstUniqueIndex 14
    |> printfn "Part two %A"