# time

let commons x =
    x
    |> Array.map set
    |> Set.intersectMany

let points (c: char) =
    match c with
    | c when int c <= int 'Z' -> int c - int 'A' + 27
    | _ -> int c - int 'a' + 1

let comparements rucksacks =
    rucksacks
    |> Array.map Seq.toArray
    |> Array.map (fun x -> Array.splitInto 2 x)

let groups rucksacks =
    rucksacks
    |> Array.chunkBySize 3
    |> Array.map (fun x -> Array.map Seq.toArray x)
    |> Array.map Seq.toArray

let solve part grouping =
    "inputs/day03.txt" 
    |> System.IO.File.ReadAllLines
    |> grouping
    |> Array.map (commons >> Set.toArray)
    |> Array.collect id
    |> Array.map points
    |> Array.sum
    |> printfn "Part %s %A" "part"

solve "one" comparements
solve "two" groups
