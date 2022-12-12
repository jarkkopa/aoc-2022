#  time 

type Item = int64
type Monkey = {
    Id: int;
    Items: Item list;
    Operations: Item -> Item;
    Test: Item -> int;
    Denominator: int64;
    Inspections: int64;
}

let parseId (line: string) =
    line.Split([|"Monkey";":"|], System.StringSplitOptions.TrimEntries)
    |> fun x -> x.[1] |> int

let parseItems (line: string) =
    line.Split(":")
    |> Array.tail
    |> Array.map (fun x -> x.Split(", "))
    |> Array.collect id
    |> Array.map int64
    |> Array.toList

let parseOperations (line: string) =
    let calc = line.Split(": new = old ")
    let parts = calc.[1].Split(" ")
    let param x =
        match parts.[1] with 
        | "old" -> x
        | _ -> parts[1] |> int64

    match parts.[0] with
    | "+" -> fun x -> x + param x
    | "*" -> fun x -> x * param x
    | _ -> failwith "Invalid operation"

let parseTest (line: string array): int64 * (Item -> int) =
    let denominator = line.[0].Split("by", System.StringSplitOptions.TrimEntries).[1] |> int64
    let toMonkey idx = line.[idx].Split("to monkey", System.StringSplitOptions.TrimEntries).[1] |> int
    let trueMonkey = toMonkey 1
    let falseMonkey = toMonkey 2
    
    (denominator, fun worryLevel -> if worryLevel % denominator = 0L then trueMonkey else falseMonkey)

let parseMonkey (monkey: string): Monkey =
    let lines =
        monkey.Split("\n", System.StringSplitOptions.TrimEntries)

    let (denominator, testFunc) = parseTest (lines |> Array.removeManyAt 0 3)
    
    { 
        Id = parseId lines.[0]
        Items = parseItems lines.[1]
        Operations = parseOperations lines.[2]
        Test = testFunc
        Denominator = denominator
        Inspections = 0L
    }

let throwItem (worryLevelModifier) (fromMonkeyIdx: int) (monkeys: Monkey array) (item: Item) =
    let fromMonkey = monkeys.[fromMonkeyIdx]
    let worryLevel = (fromMonkey.Operations item) |> worryLevelModifier
    let targetIdx = fromMonkey.Test worryLevel
    let fromIdx = monkeys |> Array.findIndex (fun m -> m.Id = fromMonkey.Id)
    let target = monkeys.[targetIdx]
    let newFromItems = monkeys.[fromIdx].Items |> List.tail

    monkeys
    |> Array.updateAt targetIdx {target with Items = target.Items @ [worryLevel]}
    |> Array.updateAt fromIdx {fromMonkey with Items = newFromItems; Inspections = (fromMonkey.Inspections + 1L)}

let runMonkeyRound worryLevelModifier (monkeys: Monkey array) (monkeyIdx: int): Monkey array =
    let updatedMonkey = monkeys.[monkeyIdx]
    updatedMonkey.Items
    |> List.scan ((throwItem worryLevelModifier) monkeyIdx) monkeys
    |> List.last

let runRound (worryLevelModifier: int64 -> int64) (monkeys: Monkey array) _: Monkey array =
    monkeys
    |> Array.map (fun m -> m.Id)
    |> Array.scan (runMonkeyRound worryLevelModifier) monkeys
    |> Array.last

let monkeys =
    "inputs/day11.txt"
    |> System.IO.File.ReadAllText
    |> fun text -> text.Split("\n\n")
    |> Array.map parseMonkey

let worryLevelDivider (worryLevel: int64) =
    worryLevel / 3L

let worryLevelModuler (nominator: int64)  (worryLevel: int64) =
    worryLevel % nominator

let smallestCommonNominator =
    monkeys
    |> Array.map (fun x -> x.Denominator)
    |> Array.fold (*) 1L

let monkeyBusinessValue (monkeys: Monkey array) =
    monkeys
    |> Array.map (fun x -> x.Inspections)
    |> Array.sortByDescending id
    |> Array.take 2
    |> Array.fold (*) 1L

[1..20]
    |> List.scan (runRound worryLevelDivider) monkeys
    |> List.last
    |> monkeyBusinessValue
    |> printfn "Part one: %A"

[1..10000]
    |> List.scan (runRound (worryLevelModuler smallestCommonNominator)) monkeys
    |> List.last
    |> monkeyBusinessValue
    |> printfn "Part two: %A"
