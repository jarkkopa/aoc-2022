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

let hasNodeBelow (floor: Y option) (nodes: NodeMap) ((x, y): Position) =
    match floor with
    | None -> 
        match nodes |> Map.tryFind x with
        | Some xMap -> 
            xMap |> Map.keys |> Seq.filter (fun yVal -> yVal > y) |> Seq.isEmpty |> not
        | None -> false
    | Some maxY -> maxY >= y

let nextFallPos (floor: Y option) (nodes: NodeMap) ((x, y): Position): Position option =
    match floor with
    | Some maxY when maxY <= y -> None
    |_ ->
        let hasNode ((nX, nY): Position) =
            nodes |> Map.tryFind nX |> Option.defaultValue Map.empty |> Map.tryFind nY

        [(x, y + 1); (x - 1, y + 1); (x + 1, y + 1)]
        |> List.map (fun pos -> 
            match hasNode pos with
            | Some _ -> None
            | _ -> Some(pos)
        )
        |> List.tryPick id

let rec pourSand (floor: Y option) (staticNodes: NodeMap) (from: Position) (node: Node) =
    let nextFallPos = nextFallPos floor staticNodes from
    match nextFallPos with
    | Some nextPos ->
        if hasNodeBelow floor staticNodes nextPos
        then pourSand floor staticNodes nextPos node
        else staticNodes
    | None ->
            addNode staticNodes {node with Type = Sand} from
            |> fun newStaticNodes -> 
                match from with
                | (500, 0) -> newStaticNodes
                | _ -> pourSand floor newStaticNodes (500, 0) {Type = Sand} 

let numOfSandToFill floor walls =
    pourSand floor walls (500, 0) {Type = Sand} 
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

numOfSandToFill None walls
    |> printfn "Part one: %A"

let floor =
    walls
    |> Map.values
    |> Seq.map Map.keys 
    |> Seq.collect id
    |> Seq.sortByDescending id
    |> Seq.head
    |> (+) 1
    |> Some

numOfSandToFill floor walls
    |> printfn "Part two: %A"