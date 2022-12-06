# time

let split (separator: string) (s: string) = s.Split separator
let isEmpty (c: char)  = c <> ' '

let toCrates containers =
    containers
    |> split "\n"
    |> Array.map Seq.toArray
    |> Array.chunkBySize 4
    |> Array.collect id
    |> Array.transpose
    |> Array.filter (Array.last >> isEmpty)
    |> Array.map (Array.filter isEmpty)
    |> Array.map (Array.rev >> Array.tail >> Array.rev)

let toMoves instructions =
    instructions
    |> split "\n"
    |> Array.map (fun x -> x.Split([|"move ";" from ";" to "|], System.StringSplitOptions.RemoveEmptyEntries ))
    |> Array.map (Array.map int)

let toCratesAndMoves input =
    input
    |> split "\n\n"
    |> (fun x -> ((toCrates x.[0]), (toMoves x.[1])))

let cratesAndMoves = 
    "inputs/day05.txt"
    |> System.IO.File.ReadAllText
    |> toCratesAndMoves

let rec moveFromTo (stack: char[][]) (amount:int) (fromIdx:int) (toIdx:int) =
    let takeSize = amount
    let items = Array.take takeSize stack.[fromIdx]
    let newFrom = Array.removeManyAt 0 takeSize stack.[fromIdx]
    let newTo = Array.insertManyAt 0 items stack.[toIdx]
    
    stack
    |> Array.updateAt fromIdx newFrom
    |> Array.updateAt toIdx newTo

let rec moveOneAtATime (amount: int) (stack: char[][]) (fromIdx:int) (toIdx:int) =
    match amount with
    | 0 -> stack
    | _ ->
        let newStack = moveFromTo stack 1 fromIdx toIdx
        moveOneAtATime (amount - 1) newStack fromIdx toIdx

let moveMultiple (amount: int) (stack: char[][]) (fromIdx:int) (toIdx:int) =
    moveFromTo stack amount fromIdx toIdx

let rec step moveMethod (crates: char[][]) (instructions: int list list)  =
    match instructions with
    | head :: tail ->
        let amount = head.[0]
        let fromIdx = head.[1] - 1
        let toIdx = head.[2] - 1
        let newCrates = moveMethod amount crates fromIdx toIdx
        step moveMethod newCrates tail
    | _ -> crates

let rearrange moveMethod (crates,instructions:int[][]) =
    instructions
    |> Array.map Array.toList
    |> Array.toList
    |> step moveMethod crates

cratesAndMoves
    |> rearrange moveOneAtATime
    |> Array.map Seq.head
    |> System.String
    |> printfn "Part one: %A"

cratesAndMoves
    |> rearrange moveMultiple
    |> Array.map Seq.head
    |> System.String
    |> printfn "Part two: %A"
