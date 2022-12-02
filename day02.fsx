# time

type Shape = Rock | Paper | Scissors
type RoundOutcome = Win | Draw | Lose

let shape x =
    match x with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | _ -> Scissors

let handPoints shape =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | _ -> 3

let roundPoints result =
    match result with
    | Win -> 6
    | Draw -> 3
    | _ -> 0

let roundResult round =
    match round with
    | [|a;b|] when a = b -> (Draw, b)
    | [|Rock; Paper|] ->(Win, Paper)
    | [|Scissors; Rock|] ->(Win, Rock)
    | [|Paper; Scissors|] ->(Win, Scissors)
    | [|_; b|] -> (Lose,b)
    | _ -> failwith "unknown shape"

"inputs/day02.txt" 
    |> System.IO.File.ReadAllLines
    |> Array.map (fun x -> x.Split(" ") )
    |> Array.map (fun x -> Array.map shape x)
    |> Array.map roundResult
    |> Array.map (fun (result, ownShape) -> roundPoints result + handPoints ownShape)
    |> Array.sum
    |> printfn "Part one: %A"
    