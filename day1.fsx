# time

let inputStr = "inputs/day01.txt" |> System.IO.File.ReadAllText

inputStr.Split("\n\n")
    |> Array.map (fun x -> x.Split("\n"))
    |> Array.map (Array.map int) 
    |> Array.map Array.sum
    |> Array.max