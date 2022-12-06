# time

"inputs/day06.txt"
    |> System.IO.File.ReadAllText
    |> Seq.toArray
    |> Array.windowed 4
    |> Array.map set
    |> Array.findIndex (fun x -> x.Count = 4)
    |> fun x -> x + 4
    |> printfn "Part one %A"