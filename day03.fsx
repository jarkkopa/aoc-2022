# time

let duplicates x =
    match x with
    | [|a; b|] -> Set.intersect (set a) (set b)
    | _ -> failwith "Invalid chunks"

let points (c: char) =
    match c with
    | c when int c <= int 'Z' -> int c - int 'A' + 27
    | _ -> int c - int 'a' + 1

"inputs/day03.txt" 
    |> System.IO.File.ReadAllLines
    |> Array.map Seq.toArray
    |> Array.map (fun x -> Array.splitInto 2 x)
    |> Array.map (duplicates >> Set.toArray)
    |> Array.collect id
    |> Array.map points
    |> Array.sum