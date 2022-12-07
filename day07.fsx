# time

type InputLine = Command | Output
type Path = string
type FileSystemEntryType = File | Directory
type FileSystemEntry = {FullPath:string; Size:int; Type:FileSystemEntryType}

let parseCommand (line:string) =
    line
    |> fun x -> x.Substring(2)
    |> fun x -> x.Split(" ")

let parseOutput (line:string) =
    line
    |> fun x -> x.Split(" ")

let input = 
    "inputs/day07.txt"
    |> System.IO.File.ReadAllLines
    |> Array.toList
    |> List.map (fun line ->
        let first = line.Substring(0, 1)
        match first with
        | "$" -> (Command, parseCommand line)
        | _ -> (Output, parseOutput line)
    )

let moveDirectory (currentPath: Path) directory =
    match currentPath with
    | "" -> "/"
    | "/" -> currentPath + directory
    | _ -> currentPath + "/" + directory

let moveBack (currentPath: Path) =
    let lastDirectoryIdx = currentPath.LastIndexOf("/")
    if lastDirectoryIdx = 0 then "/"
    else currentPath.Substring(0, lastDirectoryIdx)

let processCommand (state: Path * FileSystemEntry list) (command: string[])=
    let commandPart = command.[0]
    let path = fst state
    let fileSystem = snd state

    match commandPart with
    | "ls" ->
        state
    | "cd" -> 
        match command.[1] with
        | ".." -> (moveBack path, fileSystem)
        | directory -> (moveDirectory path directory, fileSystem)
    | _ -> failwith "Unknown command"

let processOutput (state: Path * FileSystemEntry list) (output: string[]) =
    let firstPart = output.[0]
    let path = fst state
    let fileSystem = snd state

    match firstPart with
    | "dir" -> 
        let newFileSytem = List.append fileSystem [{FullPath=(moveDirectory path output.[1]); Type=Directory; Size=0}]
        (path, newFileSytem)
    | size ->
        let newFileSytem = List.append fileSystem [{FullPath=(moveDirectory path output.[1]); Size=(size |> int); Type=File}]
        (path, newFileSytem)

let processInput (state: Path * FileSystemEntry list) (processableInput: (InputLine * string[])) = 
    match processableInput with
    | ( Command, _) -> processCommand state (snd processableInput)
    | _ -> processOutput state (snd processableInput)

let rec runCommands (state: Path * FileSystemEntry list) (commands: (InputLine * string[]) list)= 
    match commands with
    | head::tail -> 
        let newFileSystem = processInput state head
        runCommands newFileSystem tail
    | _ -> state

let sizeByDirectory (fileSystem: FileSystemEntry list) =
    let directories =
        fileSystem
        |> List.filter (fun x -> x.Type = Directory)
    
    directories
    |> List.map (fun d ->
        fileSystem
        |> List.filter (fun file ->
            file.FullPath.Contains d.FullPath 
        )
        |> List.sumBy (fun x -> x.Size)
    )

input
    |> runCommands ("", [{FullPath="/"; Size=0; Type=Directory}])
    |> snd
    |> sizeByDirectory
    |> List.filter (fun x -> x <= 100000)
    |> List.sum
    |> printfn "Part one: %A"