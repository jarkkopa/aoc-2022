# time

let input = "inputs/day01.txt" |> System.IO.File.ReadAllText

input.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n"))
    |> Array.map (Array.map int) 
    |> Array.map Array.sum
    |> Array.max