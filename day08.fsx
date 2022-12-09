# time

type Tree = {Height: int; Visible: bool}

let charToInt c = int c - int '0'

let trees =
    "inputs/day08.txt"
        |> System.IO.File.ReadAllLines
        |> Array.map (Seq.toArray >> Array.map (charToInt))

let findVisibleTrees (highest:int) (cur: Tree array) =
    let findVisibleIndices (visibles: int list, highest: int, idx: int, trees: Tree array) cur =
        let curHeight = trees.[idx].Height
        if curHeight > highest
            then (visibles @ [idx], curHeight, idx + 1, trees)
            else (visibles, highest, idx+1, trees)

    let (visibleTreeIdx, _, _, _) = 
        cur
        |> Array.fold findVisibleIndices ([], highest, 0, cur)

    (cur, visibleTreeIdx)
    ||> List.fold (fun trees i ->
        let tree = trees.[i]
        trees |> Array.updateAt i {tree with Visible = true}
    )

trees
    |> Array.map (Array.map (fun h -> {Height=h; Visible=false}))
    |> Array.map (findVisibleTrees -1)
    |> Array.map Array.rev
    |> Array.map (findVisibleTrees -1)
    |> Array.transpose
    |> Array.map (findVisibleTrees -1)
    |> Array.map Array.rev
    |> Array.map (findVisibleTrees -1)
    |> Array.collect id
    |> Array.filter (fun x -> x.Visible)
    |> Array.length
    |> printfn "Part one: %A"