# time

type X = int
type Y = int
type Position = X * Y
type NodeType = Wall | Sand
type Node = {Type: NodeType}
type NodeMap = Map<X, Map<Y, Node>>

let addNode (target: NodeMap) (node: Node) ((x, y): Position) =
    if target.ContainsKey x
    then
        let yMap = target[x] |> Map.change y (fun m -> Some(node))
        target |> Map.change x (fun m -> Some(yMap))
    else
        let yMap = Map [(y, node)]
        target |> Map.change x (fun m -> Some(yMap))

let addWall (walls: NodeMap) (vectorStrings: string array) =
    vectorStrings
    |> Array.map ((fun x -> x.Split(",")) >> Array.map int)
    |> Array.transpose
    |> Array.map Array.sort
    |> fun ranges ->
        match ranges with
        | [|[|sX; eX|]; [|sY; eY|]|] when sX = eX -> 
            [sY..eY]
            |> List.scan (fun (walls: NodeMap) (y: Y) -> 
                addNode walls {Type = Wall} (sX, y)
            ) walls
        | [|[|sX; eX|]; [|sY; eY|]|] when sY = eY -> 
            [sX..eX]
            |> List.scan (fun (walls: NodeMap) (x: X) -> 
                addNode walls {Type = Wall} (x, sY)
            ) walls
        | _ -> failwith "Diagonal wall not supported"      
    |> List.last

let walls =
    "inputs/day14.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line -> line.Split("->", System.StringSplitOptions.TrimEntries))
    |> Array.map (Array.windowed 2)
    |> Array.collect id
    |> Array.scan addWall Map.empty
    |> Array.last

let hasNodeBelow (nodes: NodeMap) ((x, y): Position) =
    match nodes |> Map.tryFind x with
    | Some xMap ->
        xMap |> Map.keys |> Seq.filter (fun yVal -> yVal > y) |> Seq.isEmpty |> not
    | None -> false

let nextFallPos (nodes: NodeMap) ((x, y): Position): Position option =
    let hasNode ((nX, nY): Position) =
        nodes |> Map.tryFind nX |> Option.defaultValue Map.empty |> Map.tryFind nY

    [(x, y + 1); (x - 1, y + 1); (x + 1, y + 1)]
    |> List.map (fun pos -> 
        match hasNode pos with
        | Some _ -> None
        | _ -> Some(pos)
    )
    |> List.tryPick id

let rec pourSand (staticNodes: NodeMap) (from: Position) (node: Node) =
    let nextFallPos = nextFallPos staticNodes from
    match nextFallPos with
    | Some nextPos ->
        if hasNodeBelow staticNodes nextPos
        then
            pourSand staticNodes nextPos node
        else
            staticNodes
    | None ->
        addNode staticNodes {node with Type = Sand} from
        |> fun newStaticNodes -> 
        pourSand newStaticNodes (500, 0) {Type = Sand} 

pourSand walls (500, 0) {Type = Sand} 
    |> (fun nodes ->
        nodes.Values
        |> Seq.map (fun ymap -> 
            ymap.Values
            |> Seq.toArray
        )
        |> Seq.toArray
        |> Array.collect id
        |> Array.filter (fun x -> x.Type = Sand)
    )
    |> Array.length
    |> printfn "Part one: %A"