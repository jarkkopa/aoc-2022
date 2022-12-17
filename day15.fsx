# time

type X = int
type Y = int
type Position = X * Y
type Beacon = {Pos: Position}
type Sensor = {Pos: Position; Closest: Beacon; SignalLengt: int}

let parsePositions (line: string): Position * Position =
    let toPos (posStr: string) =
        posStr.Split([|"x=";", y="|], System.StringSplitOptions.TrimEntries)
        |> Array.tail
        |> Array.map int
        |> fun arr -> (arr[0], arr[1])

    line.Split([|"at";": closest"|], System.StringSplitOptions.TrimEntries)
    |> fun parts ->
        let sensorPos = parts.[1] |> toPos
        let beaconPos = parts  |> Array.last |> toPos
        (sensorPos, beaconPos)

let manhattanDistance ((fX, fY): Position) ((tX, tY): Position) =
    abs (fX - tX) + abs (fY - tY)

let createSensor ((sensorPos, beaconPos): Position * Position): Sensor =
    let signal = manhattanDistance sensorPos beaconPos
    {Pos = sensorPos; SignalLengt = signal; Closest = {Pos = beaconPos}}

let sensors =
    "inputs/day15.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map parsePositions
    |> Array.map createSensor

let coverOnLine (lineY: Y) (sensor: Sensor): Set<X*Y> =
    let yDiff = abs (snd sensor.Pos - lineY)
    let covered = (sensor.SignalLengt + 1 - (1 + yDiff)) * 2 + 1

    let sensorX = fst sensor.Pos
    match covered with
    | covered when covered <= 0 -> Set.empty
    | _ ->
        [(sensorX - covered / 2)..(sensorX + covered / 2)]
        |> List.map (fun x -> (x, lineY))
        |> Set.ofList

let beacons = sensors |> Array.map (fun s -> s.Closest.Pos) |> Set.ofArray
sensors
    |> Array.map (coverOnLine 2000000)
    |> Array.reduce (+)
    |> fun covered -> Set.difference covered beacons
    |> Set.count
    |> printfn "Part one: %A"
