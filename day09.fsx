# time

type Coordinate = {X: int; Y: int}
type Knot = {Position: Coordinate; Visited: Coordinate Set}
type Direction = L | R | U | D
type Move = {Dir: Direction; Amount: int}

let startPos = {X = 0; Y = 0}
let head = { Position = startPos; Visited = Set.empty.Add(startPos)}
let tail = { Position = startPos; Visited = Set.empty.Add(startPos)}

let parseMove (line: string):Move =
    line.Split(" ")
    |> (fun s -> 
        let amount = s.[1] |> int
        match s.[0] with
        | "L" -> {Dir = L; Amount = amount}
        | "R" -> {Dir = R; Amount = amount}
        | "U" -> {Dir = U; Amount = amount}
        | _ -> {Dir = D; Amount = amount}
    )

let isTouching (a: Coordinate) (b: Coordinate) =
    a.X - b.X |> abs <=1 &&
    a.Y - b.Y |> abs <= 1

let updatePos (oldPos: Coordinate) (dir: Direction) (amount: int): Coordinate =
    match dir with
    | L -> {oldPos with X = oldPos.X - amount}
    | R -> {oldPos with X = oldPos.X + amount}
    | U -> {oldPos with Y = oldPos.Y + amount}
    | D -> {oldPos with Y = oldPos.Y - amount}

let moveByOne ((knot, move): Knot * Move) =
    let newPos = updatePos knot.Position move.Dir 1
    let newMove = {move with Amount = move.Amount - 1}
    ({knot with Position = newPos; Visited = knot.Visited.Add(newPos)}, newMove)

let moveTail (headPos: Coordinate) (tail: Knot) =
    let tailPos = tail.Position
    let newPos = 
        match (tailPos, headPos) with
        | (t, h) when t.X = h.X->
            {t with Y = h.Y + compare t.Y h.Y}
        | (t, h) when t.Y = h.Y ->
            {t with X = h.X + compare t.X h.X}
        | (t, h) when abs (t.Y - h.Y) > 1 ->
            {h with Y = h.Y + compare t.Y h.Y}
        | (t, h) when abs (t.X - h.X) > 1 ->
             {h with X = h.X + compare t.X h.X}
        |_ -> failwith "Can't move tail"
    {tail with Position = newPos; Visited = tail.Visited.Add(newPos)}

let moveBoth ((knots, move): Knot list * Move) cur =
    let head = knots |> List.head
    let tail = knots |> List.tail
    let (newHead, newMove) = moveByOne (head, move)

    let moveRest ((acc, headKnotPos): Knot list * Coordinate) (curKnot: Knot) =
        let newKnot = 
            if isTouching headKnotPos curKnot.Position
            then curKnot
            else moveTail headKnotPos curKnot
        ([newKnot] @ acc, newKnot.Position)

    let newKnots =
        tail
        |> List.fold moveRest ([], newHead.Position)
        |> fst

    ([newHead] @ newKnots, newMove)

let move (knots: Knot list) move =
    [|1..move.Amount|]
    |> Array.fold moveBoth (knots, move)
    |> fst

let knots = [head; tail]

"inputs/day09.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map parseMove
    |> Array.fold move knots
    |> List.last
    |> fun x -> x.Visited
    |> Set.toList
    |> List.length
    |> printfn "Part one: %A"
