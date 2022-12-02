# time

type Shape = Rock | Paper | Scissors
type RoundOutcome = Win | Draw | Lose

let shape x =
    match x with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | _ -> Scissors

let desiredResult x =
    match x with
    | "X" -> Lose
    | "Y" -> Draw
    | _ -> Win

let ownShape elfShape desiredResult =
    match (elfShape, desiredResult) with
    | (a, Draw) -> a
    | (a, Win) -> 
        match a with
        | Rock -> Paper
        | Paper -> Scissors
        | _ -> Rock
    | (a, Lose) ->
        match a with
        | Rock -> Scissors
        | Paper -> Rock
        | _ -> Paper

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
    |> Array.map (fun x -> x.Split(" "))
    |> Array.map ((fun x -> Array.map shape x) >> roundResult)
    |> Array.map (fun (result, ownShape) -> roundPoints result + handPoints ownShape)
    |> Array.sum
    |> printfn "Part one: %A"

"inputs/day02.txt" 
    |> System.IO.File.ReadAllLines
    |> Array.map (fun x -> x.Split(" "))
    |> Array.map (fun x -> (shape x.[0], ownShape (shape x.[0]) (desiredResult x.[1])))
    |> Array.map (fun (elf,own) -> roundResult [|elf;own|])
    |> Array.map (fun (result, ownShape) -> roundPoints result + handPoints ownShape)
    |> Array.sum
    |> printfn "Part two: %A"
