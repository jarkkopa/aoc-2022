# time

let split (separator: string) (s: string) = s.Split separator

let rangeToSections (first, last) =
    Array.init (last - first + 1) (fun x -> first + x)

let parseRange range =
    range
    |> split "-"
    |> fun x -> (int x.[0], int x.[1])

let findOverlaps sections =
    sections
    |> Array.map (Array.map set)
    |> Array.map (fun a-> (Set.isSubset a.[0] a.[1]) || Set.isSubset a.[1] a.[0])

"inputs/day04.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (split "," >> Array.map (parseRange >> rangeToSections))
    |> findOverlaps
    |> Array.filter id 
    |> Array.length
    |> printfn "Part one: %A"