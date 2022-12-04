# time

let split (separator: string) (s: string) = s.Split separator

let rangeToSections (first, last) =
    set [first..last]

let parseRange range =
    range
    |> split "-"
    |> fun x -> (int x.[0], int x.[1])

let findFullOverlaps (sections: Set<int>[]) =
    sections
    |> (fun a-> (Set.isSubset a.[0] a.[1]) || Set.isSuperset a.[0] a.[1])

let ranges =
    "inputs/day04.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (split "," >> Array.map (parseRange >> rangeToSections))

ranges
    |> Array.map findFullOverlaps
    |> Array.filter id 
    |> Array.length
    |> printfn "Part one: %A"

ranges
    |> Array.map Set.intersectMany
    |> Array.filter (Set.isEmpty >> not)
    |> Array.length
    |> printfn "Part two: %A"
