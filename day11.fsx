#  time 

type Item = int
type Monkey = {
    Id: int;
    Items: Item list;
    Operations: Item -> int;
    Test: int -> int;
    Inspections: int;
}

let parseId (line: string) =
    line.Split([|"Monkey";":"|], System.StringSplitOptions.TrimEntries)
    |> fun x -> x.[1] |> int

let parseItems (line: string) =
    line.Split(":")
    |> Array.tail
    |> Array.map (fun x -> x.Split(", "))
    |> Array.collect id
    |> Array.map int
    |> Array.toList

let parseOperations (line: string) =
    let calc = line.Split(": new = old ")
    let parts = calc.[1].Split(" ")
    let param x =
        match parts.[1] with 
        | "old" -> x
        | _ -> parts[1] |> int

    match parts.[0] with
    | "+" -> fun x -> x + param x
    | "*" -> fun x -> x * param x
    | _ -> failwith "Invalid operation"

let parseTest (line: string array): int -> int =
    let divisible = line.[0].Split("by", System.StringSplitOptions.TrimEntries).[1] |> int
    let toMonkey idx = line.[idx].Split("to monkey", System.StringSplitOptions.TrimEntries).[1] |> int
    let trueMonkey = toMonkey 1
    let falseMonkey = toMonkey 2
    
    fun worryLevel -> if worryLevel % divisible = 0 then trueMonkey else falseMonkey

let parseMonkey (monkey: string): Monkey =
    let lines =
        monkey.Split("\n", System.StringSplitOptions.TrimEntries)

    { 
        Id = parseId lines.[0]
        Items = parseItems lines.[1]
        Operations = parseOperations lines.[2]
        Test = parseTest (lines |> Array.removeManyAt 0 3)
        Inspections = 0
    }



let throwItem (fromMonkeyIdx: int) (monkeys: Monkey array) (item: Item) =
    let fromMonkey = monkeys.[fromMonkeyIdx]
    let worryLevel = (fromMonkey.Operations item) |> fun x -> x / 3
    let targetIdx = fromMonkey.Test worryLevel
    let fromIdx = monkeys |> Array.findIndex (fun m -> m.Id = fromMonkey.Id)
    let target = monkeys.[targetIdx]
    let newFromItems = monkeys.[fromIdx].Items |> List.tail

    monkeys
    |> Array.updateAt targetIdx {target with Items = target.Items @ [worryLevel]}
    |> Array.updateAt fromIdx {fromMonkey with Items = newFromItems; Inspections = (fromMonkey.Inspections + 1)}

let runMonkeyRound (monkeys: Monkey array) (monkeyIdx: int): Monkey array =
    let updatedMonkey = monkeys.[monkeyIdx]
    updatedMonkey.Items
    |> List.scan (throwItem monkeyIdx) monkeys
    |> List.last

let runRound (monkeys: Monkey array) (round: int): Monkey array =
    monkeys
    |> Array.map (fun m -> m.Id)
    |> Array.scan runMonkeyRound monkeys
    |> Array.last

let monkeys =
    "inputs/day11.txt"
    |> System.IO.File.ReadAllText
    |> fun text -> text.Split("\n\n")
    |> Array.map parseMonkey

[1..20]
    |> List.scan runRound monkeys
    |> List.last
    |> Array.map (fun x -> x.Inspections)
    |> Array.sortByDescending id
    |> Array.take 2
    |> Array.fold (*) 1
    |> printfn "Part one: %A"
