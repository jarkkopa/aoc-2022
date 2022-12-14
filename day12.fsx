# time

type Coordinate = (int*int)
type Height = char
type Node = { Pos: Coordinate; Char: char; Height: Height; Distance: int}

let MAX = System.Int32.MaxValue

let toHeight (c: char): Height =
    match c with
    | 'S' -> 'a'
    | 'E' -> 'z'
    | x -> x

let grid = 
    "inputs/day12.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map Seq.toArray
    |> Array.mapi (fun y line ->
        line
        |> Array.mapi (fun x c -> {Pos = (x, y); Char = c; Height = toHeight c; Distance = MAX})
    )
    |> array2D

let width = grid |> Array2D.length2
let height = grid |> Array2D.length1

let canMoveTo (from: Height) (toNode: Height) =
    int toNode - int from <= 1

let closestNeighbors (queue: Node array) (grid: Node[,]) (node: Node): Node list =
    [(-1, 0);(1, 0);(0, 1);(0, -1)]
    |> List.map (fun (x, y) ->
        let newPos = (fst node.Pos + x, snd node.Pos + y)
        match newPos with
        | (newX: int, newY) when 
            newX >= 0 && newX < width && newY >= 0 && newY < height -> 
                Some grid[newY, newX]
        | _ -> None
    )
    |> List.choose id
    |> List.filter (fun n -> queue |> Array.contains n)

let updateInArray (arr: Node array) (updateNode: Node) =
    let i = arr |> Array.findIndex (fun n -> n.Pos = updateNode.Pos)
    arr |> Array.updateAt i updateNode

let findShortestPaths (nodeFilter: Node -> bool) (graph: Node[,]) (startPos: Coordinate)=
    let unvisitedNodes = 
        graph 
        |> Seq.cast<Node>
        |> Seq.toArray 
        |> Array.filter (fun n -> nodeFilter n || n.Pos = startPos)
        |> Array.map (fun n -> 
            {n with Distance = if n.Pos = startPos then 0 else MAX})

    let nodes = unvisitedNodes |> Array.copy

    let closestNode (queue: Node array) =
        queue |> Array.sortBy (fun n -> n.Distance) |> Array.head

    let rec calculate (grid, unvisited, nodes): Node[,] * Node array * Node array =
        match unvisited with
        | [||] -> 
            (grid, unvisited, nodes)
        | _ ->
            let node = closestNode unvisited
            let nodeIdx = unvisited |> Array.findIndex (fun n -> n.Pos = node.Pos)
            let newQ = unvisited |> Array.removeAt nodeIdx

            let neighbors = closestNeighbors unvisited grid node
            let newNeighbors = 
                neighbors
                |> List.map (fun n -> 
                    let distance = node.Distance + 1
                    let newDistance = if canMoveTo node.Height n.Height && n.Distance > distance then distance else n.Distance
                    {n with Distance = newDistance}
                )

            let updatedPrev = 
                newNeighbors
                |> List.fold updateInArray nodes

            let updatedQ = 
                newNeighbors
                |> List.fold updateInArray newQ

            if node.Char = 'E' || node.Distance = MAX then (grid, updatedQ, updatedPrev)
            else calculate (grid, updatedQ, updatedPrev)

    calculate (graph, unvisitedNodes, nodes)
    |> (fun (_, _, p) -> p)

let allNodes = 
    grid 
    |> Seq.cast<Node>
    |> Seq.toArray

let distanceToEnd nodes =
    nodes
    |> Array.find (fun n -> n.Char = 'E')
    |> fun n -> n.Distance

allNodes
    |> Array.find (fun n -> n.Char = 'S')
    |> (fun node -> findShortestPaths (fun n -> true) grid node.Pos)
    |> distanceToEnd
    |> printfn "Part one: %A"

allNodes
    |> Array.filter (fun n -> n.Height = 'a')
    |> Array.map (fun node -> findShortestPaths (fun n -> n.Height <> 'a') grid node.Pos)
    |> Array.map distanceToEnd
    |> Array.sort
    |> Array.head
    |> printfn "Part two: %A"
    