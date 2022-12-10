# time

type Command = Noop | Addx
type Op = {Command: Command; Cycles: int; Param: int}
type Register = int
type CpuState = {
    Signals: int list;
    Cycles: int;
    X: Register;
}

let initState = {
    Signals = []; 
    Cycles = 0; 
    X = 1}

let parseOp (line: string): Op =
    let op = line.Split(" ")
    match op.[0] with
    | "noop" -> {Command = Noop; Cycles = 1; Param = 0}
    | _ -> {Command = Addx; Cycles = 2; Param = op.[1] |> int }

let runCycle ((signals, cycles, x): int list * int * Register) (cur: int) =
    let newCycles = cycles + 1
    let newSignals = 
        if newCycles % 40 = 20
        then 
            let newSignal = x * newCycles
            signals @ [newSignal]
        else signals
    (newSignals, newCycles, x)

let runOperation (cpu: CpuState) (op: Op) =
    let waitCycles = op.Cycles

    let (newSignals, newCycles, _) = 
        [1..waitCycles]
        |> List.fold runCycle (cpu.Signals, cpu.Cycles, cpu.X)

    let newX = 
        match op.Command with
        | Noop -> cpu.X
        | Addx -> cpu.X + op.Param

    {cpu with X = newX; Signals = newSignals; Cycles = newCycles}

"inputs/day10.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map parseOp
    |> Array.fold runOperation initState
    |> fun x -> x.Signals
    |> List.sum
    |> printfn "Part one: %A"
