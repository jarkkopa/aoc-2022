# time

let input = "inputs/day01.txt" |> System.IO.File.ReadAllText

let calories = 
    input.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n"))
    |> Array.map (Array.map int)
    |> Array.map Array.sum

// Part one
calories
    |> Array.max
    |> printfn "Part one: %A"

// Part two
calories
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum
    |> printfn "Part two: %A"
