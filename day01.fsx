# time

let input = "inputs/day01.txt" |> System.IO.File.ReadAllText

let calories = 
    input.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n"))
    |> Array.map (Array.map int)     

// Part one
calories
    |> Array.map Array.sum
    |> Array.max

// Part two
calories
    |> Array.sortByDescending Array.sum
    |> Array.take 3
    |> Array.map Array.sum |> Array.sum

